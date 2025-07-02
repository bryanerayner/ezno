use std::borrow::Cow;
use binary_serialize_derive;

#[derive(Debug, Clone, Eq)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct UnifiedIdentifier<'a> {
    original: &'a str,        // Original spelling (kept for debugging/round‑tripping)
    normalized: Vec<NormalizedData<'a>>, // Lower‑case words after unification
    pascal_case: once_cell::unsync::OnceCell<String>,
}

impl<'a> UnifiedIdentifier<'a> {
    /// Returns the PascalCase version of the normalized identifier.
    pub fn pascal_case(&self) -> &str {
        self.pascal_case.get_or_init(|| {
            let mut s = String::new();
            for part in &self.normalized {
                let word = match part {
                    NormalizedData::Str(w) | NormalizedData::StrWithoutHyphens(w) => {
                        let w = if let NormalizedData::StrWithoutHyphens(_) = part {
                            w.replace('-', "")
                        } else {
                            w.to_string()
                        };
                        w
                    }
                };
                let mut chars = word.chars();
                if let Some(first) = chars.next() {
                    s.push(first.to_ascii_uppercase());
                    s.extend(chars.flat_map(|c| c.to_lowercase()));
                }
            }
            s
        })
    }
}

#[derive(Debug, Clone, Eq, binary_serialize_derive::BinarySerializable)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct StringUnifiedIdentifier {
    original: String,        // Original spelling (kept for debugging/round‑tripping)
    normalized: Vec<String>, // Lower‑case words after unification
}

impl StringUnifiedIdentifier {
    pub fn original_str(&self) -> &str {
        &self.original
    }

    pub fn as_str(&self) -> &str {
        &self.original
    }

    pub fn original_string(&self) -> String {
        self.original.clone()
    }

    pub fn original_borrowed_cow(&self) -> Cow<str> {
        Cow::Borrowed(self.original.as_str())
    }
}

impl PartialEq<&str> for StringUnifiedIdentifier {
    fn eq(&self, other: &&str) -> bool {
        self.original == *other
    }
}

impl PartialEq<StringUnifiedIdentifier> for &str {
    fn eq(&self, other: &StringUnifiedIdentifier) -> bool {
        *self == other.original
    }
}

impl From<&str> for StringUnifiedIdentifier {
    fn from(s: &str) -> Self {
        StringUnifiedIdentifier::new(s)
    }
}

impl From<String> for StringUnifiedIdentifier {
    fn from(s: String) -> Self {
        StringUnifiedIdentifier::new(&s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
enum NormalizedData<'a> {
    Str(&'a str),
    StrWithoutHyphens(&'a str)
}

impl<'a> UnifiedIdentifier<'a> {
    /// Returns the identifier exactly as it appeared in the source code as a `&str`.
    pub fn as_str(&self) -> &str {
        &self.original
    }
}

/* -------------------------------------------------------------------------
 *  Pascal / camel + acronym splitter
 * ----------------------------------------------------------------------*/

/// Split identifiers that use PascalCase or camelCase (and may contain
/// acronyms) into their component lower‑case words.
///
/// Rules
/// -----
/// * `lower → UPPER` transition ⇒ new word (`fooBar`).
/// * `UPPER* → UpperLower{2+}` ⇒ split before last UPPER so that
///   `HTTPServer` ⇒ `HTTP` + `Server` but `APIs` stays whole.
/// * Letter → digit transition ⇒ new word (`GL3DModel`).
fn split_pascal_case<'a>(s: &'a str) -> Vec<NormalizedData<'a>> {
    let chars: Vec<char> = s.chars().collect();
    let mut words = Vec::new();
    let mut start = 0;
    let len = chars.len();

    let mut i = 1;
    while i <= len {
        let boundary = if i == len {
            true
        } else {
            let ch = chars[i];
            let prev = chars[i - 1];

            if ch.is_uppercase() {
                if prev.is_lowercase() {
                    true
                } else if prev.is_uppercase() {
                    if let Some(&next) = chars.get(i + 1) {
                        if next.is_lowercase() {
                            // count subsequent lowers ≥2 → boundary
                            let mut j = i + 1;
                            let mut lowers = 0;
                            while j < len && chars[j].is_lowercase() {
                                lowers += 1;
                                j += 1;
                            }
                            lowers > 1
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else if ch.is_ascii_digit() && !prev.is_ascii_digit() {
                true
            } else {
                false
            }
        };

        if boundary {
            if start < i {
                let word = &s[start..i];
                words.push(NormalizedData::Str(word));
            }
            start = i;
        }
        i += 1;
    }

    // Lowercase all words and wrap as NormalizedData::Str
    words
        .into_iter()
        .map(|nd| match nd {
            NormalizedData::Str(w) => NormalizedData::Str(w),
            _ => nd,
        })
        .collect()
}

/* -------------------------------------------------------------------------
 *  High‑level normaliser
 * ----------------------------------------------------------------------*/

/// Convert an identifier (in *any* supported spelling style) into a canonical
/// list of lower‑case words so that syntactically different but logically
/// equivalent identifiers compare equal.
///
/// Accepted styles
/// ---------------
/// 1. **Space/underscore separated** – split on space/underscore only.
///    Hyphens remain literal, because a mix of “words + - char” can only
///    appear in *broken* identifiers (they will then fail to compare equal to
///    any well‑formed spelling).
/// 2. **Pure kebab‑case** – *must* contain a `-`, **and** contain *neither*
///    spaces nor underscores.  Split on `-`.
/// 3. Otherwise, treat as Pascal/camel (possibly with acronyms).
fn normalize<'a>(id: &'a str) -> Vec<NormalizedData<'a>> {
    if id.contains(' ') || id.contains('_') {
        // Style #1 – tokens separated by space/underscore. We *also* fold away
        // internal hyphens **except** when the token *starts* with a hyphen or
        // is exactly "-".  This lets brand names like "Coca‑Cola" compare
        // equal to "Coca Cola", while still distinguishing pathological
        // spellings such as "-the-kebab".
        id.split(|c| c == ' ' || c == '_')
            .filter_map(|t| {
                if t.is_empty() {
                    None
                } else if t.starts_with('-') || t.ends_with('-') || t == "-" {
                    // Leading *or* trailing hyphen ⇒ syntactically significant → keep.
                    Some(NormalizedData::Str(t))
                } else {
                    // Internal brand‑style hyphens can be erased.
                    Some(NormalizedData::StrWithoutHyphens(t))
                }
            })
            .collect()
    } else if id.contains('-') {
        // Style #2 – strict kebab (no spaces/underscores allowed).
        id.split('-')
            .filter(|s| !s.is_empty())
            .map(|s| NormalizedData::Str(s))
            .collect()
    } else {
        // Style #3 – Pascal/camel/acronyms.
        split_pascal_case(id)
    }
}

impl<'a> UnifiedIdentifier<'a> {
    pub fn new(original: &'a str) -> Self {
        Self {
            original: original,
            normalized: normalize(original),
            pascal_case: once_cell::unsync::OnceCell::new(),
        }
    }

    /// Returns the identifier exactly as it appeared in the source code.
    #[must_use]
    pub fn original(&self) -> &str {
        &self.original
    }
}

// impl<'a> UnifiedIdentifier<'a> {
//     /// Helper: concatenate normalized pieces without separators.  This gives a
//     /// canonical string that ignores stylistic word boundaries but *preserves*
//     /// internal punctuation such as hyphens (important for distinguishing
//     /// `one-variable` from `onevariable`).
//     fn squash(&self) -> String {
//         self.normalized.join("")
//     }
// }

impl<'a> PartialEq for UnifiedIdentifier<'a> {
    /// Two identifiers are considered equal if their *squashed* (fully
    /// concatenated, lower‑case) forms match.  This makes
    ///
    /// ```text
    /// ALL_UPPER_CASE      == all upperCase
    /// Ride Shark Unified  == RideShark unifiedMobility
    /// ```
    ///
    /// while still keeping `one-variable` distinct from `onevariable` because
    /// the hyphen is retained inside tokens when it is syntactically
    /// significant.
    fn eq(&self, other: &Self) -> bool {
        if self.normalized.len() != other.normalized.len() {
            return false;
        }

        for (a, b) in self.normalized.iter().zip(other.normalized.iter()) {
            let (a_str, a_strip) = match a {
                NormalizedData::Str(s) => (*s, false),
                NormalizedData::StrWithoutHyphens(s) => (*s, true),
            };
            let (b_str, b_strip) = match b {
                NormalizedData::Str(s) => (*s, false),
                NormalizedData::StrWithoutHyphens(s) => (*s, true),
            };

            let a_cmp = if a_strip { a_str.replace('-', "") } else { a_str.to_string() };
            let b_cmp = if b_strip { b_str.replace('-', "") } else { b_str.to_string() };

            if !a_cmp.eq_ignore_ascii_case(&b_cmp) {
                return false;
            }
        }

        true
    }
}





/* -------------------------------------------------------------------------
 *  Pascal / camel + acronym splitter
 * ----------------------------------------------------------------------*/

/// Split identifiers that use PascalCase or camelCase (and may contain
/// acronyms) into their component lower‑case words.
///
/// Rules
/// -----
/// * `lower → UPPER` transition ⇒ new word (`fooBar`).
/// * `UPPER* → UpperLower{2+}` ⇒ split before last UPPER so that
///   `HTTPServer` ⇒ `HTTP` + `Server` but `APIs` stays whole.
/// * Letter → digit transition ⇒ new word (`GL3DModel`).
fn string_split_pascal_case(s: &str) -> Vec<String> {
    let chars: Vec<char> = s.chars().collect();
    let mut words = Vec::<String>::new();
    let mut cur = String::new();

    for i in 0..chars.len() {
        let ch = chars[i];

        if !cur.is_empty() {
            let prev = chars[i - 1];
            let mut boundary = false;

            if ch.is_uppercase() {
                if prev.is_lowercase() {
                    boundary = true;
                } else if prev.is_uppercase() {
                    if let Some(&next) = chars.get(i + 1) {
                        if next.is_lowercase() {
                            // count subsequent lowers ≥2 → boundary
                            let mut j = i + 1;
                            let mut lowers = 0;
                            while j < chars.len() && chars[j].is_lowercase() {
                                lowers += 1;
                                j += 1;
                            }
                            if lowers > 1 {
                                boundary = true;
                            }
                        }
                    }
                }
            } else if ch.is_ascii_digit() && !prev.is_ascii_digit() {
                boundary = true;
            }

            if boundary {
                words.push(cur.to_ascii_lowercase());
                cur.clear();
            }
        }

        cur.push(ch);
    }

    if !cur.is_empty() {
        words.push(cur.to_ascii_lowercase());
    }

    words
}

/* -------------------------------------------------------------------------
 *  High‑level normaliser
 * ----------------------------------------------------------------------*/

/// Convert an identifier (in *any* supported spelling style) into a canonical
/// list of lower‑case words so that syntactically different but logically
/// equivalent identifiers compare equal.
///
/// Accepted styles
/// ---------------
/// 1. **Space/underscore separated** – split on space/underscore only.
///    Hyphens remain literal, because a mix of “words + - char” can only
///    appear in *broken* identifiers (they will then fail to compare equal to
///    any well‑formed spelling).
/// 2. **Pure kebab‑case** – *must* contain a `-`, **and** contain *neither*
///    spaces nor underscores.  Split on `-`.
/// 3. Otherwise, treat as Pascal/camel (possibly with acronyms).
fn string_normalize(id: &str) -> Vec<String> {
    if id.contains(' ') || id.contains('_') {
        // Style #1 – tokens separated by space/underscore. We *also* fold away
        // internal hyphens **except** when the token *starts* with a hyphen or
        // is exactly "-".  This lets brand names like "Coca‑Cola" compare
        // equal to "Coca Cola", while still distinguishing pathological
        // spellings such as "-the-kebab".
        id.split(|c| c == ' ' || c == '_')
            .filter_map(|t| {
                if t.is_empty() {
                    None
                } else if t.starts_with('-') || t.ends_with('-') || t == "-" {
                    // Leading *or* trailing hyphen ⇒ syntactically significant → keep.
                    Some(t.to_ascii_lowercase())
                } else {
                    // Internal brand‑style hyphens can be erased.
                    Some(t.replace('-', "").to_ascii_lowercase())
                }
            })
            .collect()
    } else if id.contains('-') {
        // Style #2 – strict kebab (no spaces/underscores allowed).
        id.split('-')
            .filter(|s| !s.is_empty())
            .map(|s| s.to_ascii_lowercase())
            .collect()
    } else {
        // Style #3 – Pascal/camel/acronyms.
        string_split_pascal_case(id)
    }
}

impl StringUnifiedIdentifier {
    pub fn new(original: &str) -> Self {
        Self {
            original: original.to_owned(),
            normalized: string_normalize(original),
        }
    }
}

impl StringUnifiedIdentifier {
    /// Helper: concatenate normalized pieces without separators.  This gives a
    /// canonical string that ignores stylistic word boundaries but *preserves*
    /// internal punctuation such as hyphens (important for distinguishing
    /// `one-variable` from `onevariable`).
    fn squash(&self) -> String {
        self.normalized.join("")
    }
}

impl PartialEq for StringUnifiedIdentifier {
    /// Two identifiers are considered equal if their *squashed* (fully
    /// concatenated, lower‑case) forms match.  This makes
    ///
    /// ```text
    /// ALL_UPPER_CASE      == all upperCase
    /// Ride Shark Unified  == RideShark unifiedMobilityAdd commentMore actions
    /// ```
    ///
    /// while still keeping `one-variable` distinct from `onevariable` because
    /// the hyphen is retained inside tokens when it is syntactically
    /// significant.
    fn eq(&self, other: &Self) -> bool {
        self.squash() == other.squash()
    }
}



// ---- in the same module as the two identifier types ----------------------

impl<'a> From<&'a StringUnifiedIdentifier> for UnifiedIdentifier<'a> {
    fn from(src: &'a StringUnifiedIdentifier) -> Self {
        let normalized = src
            .normalized
            .iter()
            .map(|piece| NormalizedData::Str(piece.as_str()))
            .collect();

        Self {
            original: &src.original,
            normalized,
        }
    }
}

// (optional but ergonomic) --------------------------------------------------
impl StringUnifiedIdentifier {
    /// Borrow `self` as a `UnifiedIdentifier` without cloning.
    pub fn as_unified(&self) -> UnifiedIdentifier<'_> {
        UnifiedIdentifier::from(self)
    }
}

#[cfg(feature = "self-rust-tokenize")]
impl<'a> self_rust_tokenize::SelfRustTokenize for UnifiedIdentifier<'a> {
    fn append_to_token_stream(
        &self,
        token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
    ) {
        let original = self.original;
        token_stream.extend(
            self_rust_tokenize::quote!(
                ezno_parser::types::unified_identifier::UnifiedIdentifier::new(#original)
            ),
        );
    }
}

#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for StringUnifiedIdentifier {
    fn append_to_token_stream(
        &self,
        token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
    ) {
        let original = self.original.as_str();
        token_stream.extend(
            self_rust_tokenize::quote!(
                ezno_parser::types::unified_identifier::StringUnifiedIdentifier::new(#original)
            ),
        );
    }
}



#[cfg(test)]
mod str_tests {
    use super::*;

    /* ---------- canonical equivalence sets ---------- */

    #[test]
    fn one_variable_equivalence() {
        let kebab = UnifiedIdentifier::new("one-variable");
        let snake = UnifiedIdentifier::new("one_variable");
        let spaced = UnifiedIdentifier::new("one variable");
        let camel = UnifiedIdentifier::new("oneVariable");
        assert_eq!(kebab, snake);
        assert_eq!(kebab, spaced);
        assert_eq!(kebab, camel);
    }

    /* ---------- previously existing tests ---------- */

    #[test]
    fn acronym_plural() {
        let a = UnifiedIdentifier::new("APIs");
        let b = UnifiedIdentifier::new("apis");
        assert_eq!(a, b);
        assert_eq!(a.normalized, vec![NormalizedData::Str("apis")]);
    }

    #[test]
    fn pascal_case() {
        let a = UnifiedIdentifier::new("InitialHTTPRequest");
        let b = UnifiedIdentifier::new("initial http request");
        assert_eq!(a, b);
    }

    #[test]
    fn camel_case() {
        let a = UnifiedIdentifier::new("initialHttpRequest");
        let b = UnifiedIdentifier::new("initial http request");
        assert_eq!(a, b);
    }

    #[test]
    fn xml_http() {
        let a = UnifiedIdentifier::new("XMLHttpRequest");
        assert_eq!(a.normalized, vec![
            NormalizedData::Str("xml"),
            NormalizedData::Str("http"),
            NormalizedData::Str("request")
            ]);
    }

    #[test]
    fn ids_plural() {
        let a = UnifiedIdentifier::new("IDs");
        let b = UnifiedIdentifier::new("ids");
        assert_eq!(a, b);
    }

    #[test]
    fn http_server() {
        let a = UnifiedIdentifier::new("HTTPServer");
        let b = UnifiedIdentifier::new("http server");
        assert_eq!(a, b);
    }

    #[test]
    fn kebab_whole() {
        let a = UnifiedIdentifier::new("when-the-kebab-is-whole-it-is-one-identifier");
        let b = UnifiedIdentifier::new("when the kebab is whole it is one identifier");
        assert_eq!(a, b);
    }

    /* ---------- negative cases ---------- */

    #[test]
    fn kebab_broken_invalid() {
        let broken = UnifiedIdentifier::new("when the kebab - is - broken");
        let fixed  = UnifiedIdentifier::new("when the kebab is broken");
        assert_ne!(broken, fixed);

        let broken = UnifiedIdentifier::new("when the kebab -is - broken");
        let fixed  = UnifiedIdentifier::new("when the kebab is broken");
        assert_ne!(broken, fixed);

        let broken = UnifiedIdentifier::new("when-the-kebab-is-broken-");
        let fixed  = UnifiedIdentifier::new("when the kebab is broken");
        assert_eq!(broken, fixed);

        let broken = UnifiedIdentifier::new("when -the-kebab-is-broken");
        let fixed  = UnifiedIdentifier::new("when the kebab is broken");
        assert_ne!(broken, fixed);

    }

    #[test]
    fn mixed_snake_kebab() {
        let mixed = UnifiedIdentifier::new("one-variable_foo");
        let canonical = UnifiedIdentifier::new("one variable foo");
        assert_eq!(mixed, canonical);
    }

    #[test]
    fn mixed_snake_kebab_with_space() {
        let mixed = UnifiedIdentifier::new("one- variable_foo");
        let canonical = UnifiedIdentifier::new("one variable foo");
        assert_ne!(mixed, canonical);

        let mixed = UnifiedIdentifier::new("one - variable_foo");
        let canonical = UnifiedIdentifier::new("one variable foo");
        assert_ne!(mixed, canonical);

        let mixed = UnifiedIdentifier::new("one -variable_foo");
        let canonical = UnifiedIdentifier::new("one variable foo");
        assert_ne!(mixed, canonical);
    }

    #[test]
    fn mixed_upper_case() {
        let mixed = UnifiedIdentifier::new("ALL_UPPER_CASE");
        let canonical = UnifiedIdentifier::new("allUPPERCase");
        assert_eq!(mixed, canonical);

        let mixed = UnifiedIdentifier::new("ALL_UPPER_CASE");
        let canonical = UnifiedIdentifier::new("All upper case");
        assert_eq!(mixed, canonical);

        let mixed = UnifiedIdentifier::new("ALL_UPPER_CASE");
        let canonical = UnifiedIdentifier::new("all upperCase");
        assert_eq!(mixed, canonical);
    }

    #[test]
    fn camel_cased_brand_names() {
        let mixed = UnifiedIdentifier::new("RideShark unified mobility");
        let canonical = UnifiedIdentifier::new("Ride Shark Unified Mobility");
        assert_eq!(mixed, canonical);

        let mixed = UnifiedIdentifier::new("RideShark unifiedMobility");
        let canonical = UnifiedIdentifier::new("RideSharkUnifiedMobility");
        assert_eq!(mixed, canonical);

        let mixed = UnifiedIdentifier::new("Ride Shark Unified_mobility");
        let canonical = UnifiedIdentifier::new("ridesharkunifiedmobility");
        assert_eq!(mixed, canonical);
    }

    #[test]
    fn hyphenated_brand_names_work() {
        let o1 = UnifiedIdentifier::new("Coca-Cola Seasonal Promotions");
        let o2 = UnifiedIdentifier::new("coca cola seasonal promotions");
        let o3 = UnifiedIdentifier::new("CocaColaSeasonalPromotions");
        let o4 = UnifiedIdentifier::new("COCACOLA_SeasonalPromotions");
        let o5 = UnifiedIdentifier::new("CocaColaSeasonalPromotions");
        assert_eq!(o1, o1);
        assert_eq!(o1, o2);
        assert_eq!(o1, o3);
        assert_eq!(o1, o4);
        assert_eq!(o1, o5);

        assert_eq!(o2, o2);
        assert_eq!(o2, o3);
        assert_eq!(o2, o4);
        assert_eq!(o2, o5);
        
        assert_eq!(o3, o3);
        assert_eq!(o3, o4);
        assert_eq!(o3, o5);
        
        assert_eq!(o4, o4);
        assert_eq!(o4, o5);
        
        assert_eq!(o5, o5);        
    }

    #[test]
    fn digit_boundary() {
        let a = UnifiedIdentifier::new("GL3DModel");
        assert_eq!(a.normalized, vec![
            NormalizedData::Str("gl"),
            NormalizedData::Str("3d"),
            NormalizedData::Str("model")
            ]);
    }
}

#[cfg(test)]
mod string_tests {
    use super::*;

    /* ---------- canonical equivalence sets ---------- */

    #[test]
    fn one_variable_equivalence() {
        let kebab = StringUnifiedIdentifier::new("one-variable");
        let snake = StringUnifiedIdentifier::new("one_variable");
        let spaced = StringUnifiedIdentifier::new("one variable");
        let camel = StringUnifiedIdentifier::new("oneVariable");
        assert_eq!(kebab, snake);
        assert_eq!(kebab, spaced);
        assert_eq!(kebab, camel);
    }

    /* ---------- previously existing tests ---------- */

    #[test]
    fn acronym_plural() {
        let a = StringUnifiedIdentifier::new("APIs");
        let b = StringUnifiedIdentifier::new("apis");
        assert_eq!(a, b);
        assert_eq!(a.normalized, vec!["apis"]);
    }

    #[test]
    fn pascal_case() {
        let a = StringUnifiedIdentifier::new("InitialHTTPRequest");
        let b = StringUnifiedIdentifier::new("initial http request");
        assert_eq!(a, b);
    }

    #[test]
    fn camel_case() {
        let a = StringUnifiedIdentifier::new("initialHttpRequest");
        let b = StringUnifiedIdentifier::new("initial http request");
        assert_eq!(a, b);
    }

    #[test]
    fn xml_http() {
        let a = StringUnifiedIdentifier::new("XMLHttpRequest");
        assert_eq!(a.normalized, vec![
            "xml",
            "http",
            "request"
            ]);
    }

    #[test]
    fn ids_plural() {
        let a = StringUnifiedIdentifier::new("IDs");
        let b = StringUnifiedIdentifier::new("ids");
        assert_eq!(a, b);
    }

    #[test]
    fn http_server() {
        let a = StringUnifiedIdentifier::new("HTTPServer");
        let b = StringUnifiedIdentifier::new("http server");
        assert_eq!(a, b);
    }

    #[test]
    fn kebab_whole() {
        let a = StringUnifiedIdentifier::new("when-the-kebab-is-whole-it-is-one-identifier");
        let b = StringUnifiedIdentifier::new("when the kebab is whole it is one identifier");
        assert_eq!(a, b);
    }

    /* ---------- negative cases ---------- */

    #[test]
    fn kebab_broken_invalid() {
        let broken = StringUnifiedIdentifier::new("when the kebab - is - broken");
        let fixed  = StringUnifiedIdentifier::new("when the kebab is broken");
        assert_ne!(broken, fixed);

        let broken = StringUnifiedIdentifier::new("when the kebab -is - broken");
        let fixed  = StringUnifiedIdentifier::new("when the kebab is broken");
        assert_ne!(broken, fixed);

        let broken = StringUnifiedIdentifier::new("when-the-kebab-is-broken-");
        let fixed  = StringUnifiedIdentifier::new("when the kebab is broken");
        assert_eq!(broken, fixed);

        let broken = StringUnifiedIdentifier::new("when -the-kebab-is-broken");
        let fixed  = StringUnifiedIdentifier::new("when the kebab is broken");
        assert_ne!(broken, fixed);

    }

    #[test]
    fn mixed_snake_kebab() {
        let mixed = StringUnifiedIdentifier::new("one-variable_foo");
        let canonical = StringUnifiedIdentifier::new("one variable foo");
        assert_eq!(mixed, canonical);
    }

    #[test]
    fn mixed_snake_kebab_with_space() {
        let mixed = StringUnifiedIdentifier::new("one- variable_foo");
        let canonical = StringUnifiedIdentifier::new("one variable foo");
        assert_ne!(mixed, canonical);

        let mixed = StringUnifiedIdentifier::new("one - variable_foo");
        let canonical = StringUnifiedIdentifier::new("one variable foo");
        assert_ne!(mixed, canonical);

        let mixed = StringUnifiedIdentifier::new("one -variable_foo");
        let canonical = StringUnifiedIdentifier::new("one variable foo");
        assert_ne!(mixed, canonical);
    }

    #[test]
    fn mixed_upper_case() {
        let mixed = StringUnifiedIdentifier::new("ALL_UPPER_CASE");
        let canonical = StringUnifiedIdentifier::new("allUPPERCase");
        assert_eq!(mixed, canonical);

        let mixed = StringUnifiedIdentifier::new("ALL_UPPER_CASE");
        let canonical = StringUnifiedIdentifier::new("All upper case");
        assert_eq!(mixed, canonical);

        let mixed = StringUnifiedIdentifier::new("ALL_UPPER_CASE");
        let canonical = StringUnifiedIdentifier::new("all upperCase");
        assert_eq!(mixed, canonical);
    }

    #[test]
    fn camel_cased_brand_names() {
        let mixed = StringUnifiedIdentifier::new("RideShark unified mobility");
        let canonical = StringUnifiedIdentifier::new("Ride Shark Unified Mobility");
        assert_eq!(mixed, canonical);

        let mixed = StringUnifiedIdentifier::new("RideShark unifiedMobility");
        let canonical = StringUnifiedIdentifier::new("RideSharkUnifiedMobility");
        assert_eq!(mixed, canonical);

        let mixed = StringUnifiedIdentifier::new("Ride Shark Unified_mobility");
        let canonical = StringUnifiedIdentifier::new("ridesharkunifiedmobility");
        assert_eq!(mixed, canonical);
    }

    #[test]
    fn hyphenated_brand_names_work() {
        let o1 = StringUnifiedIdentifier::new("Coca-Cola Seasonal Promotions");
        let o2 = StringUnifiedIdentifier::new("coca cola seasonal promotions");
        let o3 = StringUnifiedIdentifier::new("CocaColaSeasonalPromotions");
        let o4 = StringUnifiedIdentifier::new("COCACOLA_SeasonalPromotions");
        let o5 = StringUnifiedIdentifier::new("CocaColaSeasonalPromotions");
        assert_eq!(o1, o1);
        assert_eq!(o1, o2);
        assert_eq!(o1, o3);
        assert_eq!(o1, o4);
        assert_eq!(o1, o5);

        assert_eq!(o2, o2);
        assert_eq!(o2, o3);
        assert_eq!(o2, o4);
        assert_eq!(o2, o5);
        
        assert_eq!(o3, o3);
        assert_eq!(o3, o4);
        assert_eq!(o3, o5);
        
        assert_eq!(o4, o4);
        assert_eq!(o4, o5);
        
        assert_eq!(o5, o5);        
    }

    #[test]
    fn digit_boundary() {
        let a = StringUnifiedIdentifier::new("GL3DModel");
        assert_eq!(a.normalized, vec![
            "gl",
            "3d",
            "model"
            ]);
    }
}
