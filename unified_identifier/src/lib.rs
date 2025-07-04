//! Unified identifier types – owned (`UnifiedIdentifierBuf`) and borrowed (`UnifiedIdentifier`)
//! Similar to `PathBuf` / `Path` in the std‑lib, plus *canonical‑name* cache.

use std::{borrow::Cow, fmt, hash::{Hash, Hasher}, ops::Deref};
use once_cell::unsync::OnceCell;

/* -------------------------------------------------------------------------
 *  Normalised data representation (borrowed)
 * ---------------------------------------------------------------------*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NormalizedPart<'a> {
    /// Word kept *exactly* as in the original slice (but compared case‑insensitively)
    Str(&'a str),
    /// Word where internal hyphens are **ignored** for equality / hashing
    StrNoHyphens(&'a str),
}

/* -------------------------------------------------------------------------
 *  Owned buffer – like `PathBuf`
 * ---------------------------------------------------------------------*/
#[derive(Clone, Debug, Eq)]
pub struct UnifiedIdentifierBuf {
    original:   String,             // spelling as written by the user
    normalized: Vec<String>,        // canonical lower‑case words (owned)

    canonical:  OnceCell<String>,   // cached canonical name (Pascal/camel)
}

impl UnifiedIdentifierBuf {
    /* ----- ctor & accessors -------------------------------------------*/
    pub fn new<S: Into<String>>(s: S) -> Self {
        let original = s.into();
        let normalized = string_normalize(&original);
        Self { original, normalized, canonical: OnceCell::new() }
    }

    /// Borrow as an *unsized* `UnifiedIdentifier` (zero‑cost)
    pub fn as_id(&self) -> UnifiedIdentifier<'_> { UnifiedIdentifier::from(self) }

    /// Spelling exactly as author wrote it.
    pub fn original(&self) -> &str { &self.original }

    /// Canonical name (PascalCase with optional lower‑first) – cached.
    pub fn canonical_name(&self) -> &str {
        self.canonical.get_or_init(|| canonical_name(&self.original, &self.normalized)).as_str()
    }

    /// True string comparison *ignoring* style
    fn squash(&self) -> String { self.normalized.join("") }
}

/* ----- Borrow & display plumbing ---------------------------------------*/
impl Deref for UnifiedIdentifierBuf { type Target = str; fn deref(&self) -> &str { &self.original } }
impl AsRef<str> for UnifiedIdentifierBuf { fn as_ref(&self) -> &str { &self.original } }
impl fmt::Display for UnifiedIdentifierBuf { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.original.fmt(f) } }

/* ----- Equality / Hash on canonical form -------------------------------*/
impl PartialEq for UnifiedIdentifierBuf { fn eq(&self, other: &Self) -> bool { self.squash() == other.squash() } }
impl Hash for UnifiedIdentifierBuf { fn hash<H: Hasher>(&self, state: &mut H) { self.squash().hash(state) } }

/* ----- String ↔ UID conversions ---------------------------------------*/
impl From<String>      for UnifiedIdentifierBuf { fn from(s: String)   -> Self { Self::new(s) } }
impl<'a> From<&'a str> for UnifiedIdentifierBuf { fn from(s: &'a str) -> Self { Self::new(s) } }
impl From<UnifiedIdentifierBuf> for String      { fn from(id: UnifiedIdentifierBuf) -> Self { id.original } }

/* -------------------------------------------------------------------------
 *  Borrowed view – like `Path`
 * ---------------------------------------------------------------------*/
#[derive(Debug, Clone)]
pub struct UnifiedIdentifier<'a> {
    original:   &'a str,
    normalized: Vec<NormalizedPart<'a>>,  // rebuilt cheaply – tiny
    canonical:  OnceCell<String>,
}

impl<'a> UnifiedIdentifier<'a> {
    pub fn original(&self) -> &str { self.original }
    pub fn canonical_name(&self) -> &str { self.canonical.get_or_init(|| canonical_name(self.original, &string_normalize(self.original))).as_str() }
}

/* ----- Trait plumbing --------------------------------------------------*/
impl<'a> Deref for UnifiedIdentifier<'a> { type Target = str; fn deref(&self) -> &str { self.original } }
impl<'a> AsRef<str> for UnifiedIdentifier<'a> { fn as_ref(&self) -> &str { self.original } }
impl<'a> fmt::Display for UnifiedIdentifier<'a> { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.original.fmt(f) } }

impl<'a> PartialEq for UnifiedIdentifier<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.normalized.iter().map(part_cmp).eq(other.normalized.iter().map(part_cmp))
    }
}
impl<'a> Eq for UnifiedIdentifier<'a> {}
impl<'a> Hash for UnifiedIdentifier<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) { for p in &self.normalized { part_cmp(p).hash(state) } }
}

/* ----- Conversions -----------------------------------------------------*/
impl<'a> From<&'a UnifiedIdentifierBuf> for UnifiedIdentifier<'a> {
    fn from(buf: &'a UnifiedIdentifierBuf) -> Self {
        Self {
            original:   &buf.original,
            normalized: buf.normalized.iter().map(|s| NormalizedPart::Str(s)).collect(),
            canonical:  OnceCell::new(),
        }
    }
}

/* -------------------------------------------------------------------------
 *  Helpers – canonical name & normalisation
 * ---------------------------------------------------------------------*/
fn canonical_name(original: &str, normalized: &[String]) -> String {
    let pascal = to_pascal(normalized);
    if first_alpha_is_lower(original) { lowercase_first(&pascal) } else { pascal }
}

fn to_pascal(words: &[String]) -> String {
    let mut out = String::new();
    for w in words {
        let mut ch = w.chars();
        if let Some(f) = ch.next() {
            out.push(f.to_ascii_uppercase());
            out.extend(ch.map(|c| c.to_ascii_lowercase()));
        }
    }
    out
}

fn first_alpha_is_lower(s: &str) -> bool { s.chars().find(|c| c.is_ascii_alphabetic()).map_or(false, |c| c.is_ascii_lowercase()) }
fn lowercase_first(s: &str) -> String { let mut it = s.chars(); match it.next() { Some(c) => c.to_ascii_lowercase().to_string() + it.as_str(), None => String::new() } }

fn part_cmp<'a>(p: &NormalizedPart<'a>) -> Cow<'a, str> {
    match p {
        NormalizedPart::Str(s)          => Cow::Borrowed(*s),
        NormalizedPart::StrNoHyphens(s) => Cow::Owned(s.replace('-', "")),
    }
}

/* -------------------------------------------------------------------------
 *  Normalisation logic (string → Vec<String>) – matches original tests
 * ---------------------------------------------------------------------*/
fn string_split_pascal_case(s: &str) -> Vec<String> {
    let chars: Vec<char> = s.chars().collect();
    let mut words = Vec::new();
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
                        if next.is_lowercase() { boundary = true; }
                    }
                }
            } else if ch.is_ascii_digit() && !prev.is_ascii_digit() { boundary = true; }

            if boundary { words.push(cur.to_ascii_lowercase()); cur = String::new(); }
        }

        cur.push(ch);
    }

    if !cur.is_empty() { words.push(cur.to_ascii_lowercase()); }
    words
}

fn string_normalize(id: &str) -> Vec<String> {
    if id.contains(' ') || id.contains('_') {
        id.split(|c| c == ' ' || c == '_')
            .filter_map(|t| if t.is_empty() {
                None
            } else if t.starts_with('-') || t.ends_with('-') || t == "-" {
                Some(t.to_ascii_lowercase())
            } else {
                Some(t.replace('-', "").to_ascii_lowercase())
            })
            .collect()
    } else if id.contains('-') {
        id.split('-').filter(|s| !s.is_empty()).map(|s| s.to_ascii_lowercase()).collect()
    } else {
        string_split_pascal_case(id)
    }
}

/* -------------------------------------------------------------------------
 *  Unit tests (ported from the original spec)
 * ---------------------------------------------------------------------*/
#[cfg(test)]
mod tests {
    use super::*;

    /* ---------- helper macros ---------- */
    macro_rules! uidb { ($s:expr) => { UnifiedIdentifierBuf::new($s) }; }

    /* ---------- canonical equivalence (borrowed + owned) ---------- */
    #[test] fn one_variable_equivalence() {
        let kebab  = uidb!("one-variable");
        let snake  = uidb!("one_variable");
        let spaced = uidb!("one variable");
        let camel  = uidb!("oneVariable");
        assert_eq!(kebab.as_id(), snake.as_id());
        assert_eq!(kebab.as_id(), spaced.as_id());
        assert_eq!(kebab.as_id(), camel.as_id());

        // same for owned buffers
        let kebab  = uidb!("one-variable");
        let snake  = uidb!("one_variable");
        let spaced = uidb!("one variable");
        let camel  = uidb!("oneVariable");
        assert_eq!(kebab, snake);
        assert_eq!(kebab, spaced);
        assert_eq!(kebab, camel);
    }

    /* ---------- hyphenated brand names ---------- */
    #[test] fn coca_cola_equivalence() {
        let forms = [
            "Coca-Cola Seasonal Promotions",
            "coca cola seasonal promotions",
            "CocaColaSeasonalPromotions",
            "COCACOLA_SeasonalPromotions",
            "CocaColaSeasonalPromotions",
        ];
        for &a in &forms {
            for &b in &forms {
                let aa  = uidb!(a);
                let bb  = uidb!(b);
                assert_eq!(aa.as_id(), bb.as_id());
                assert_eq!(aa, bb);
            }
        }
    }

    /* ---------- negative cases ---------- */
    #[test] fn mixed_snake_kebab_with_space_not_equal() {
        let bad1 = uidb!("one- variable_foo");
        let bad2 = uidb!("one - variable_foo");
        let bad3 = uidb!("one -variable_foo");
        let canonical = uidb!("one variable foo");
        assert_ne!(bad1, canonical);
        assert_ne!(bad2, canonical);
        assert_ne!(bad3, canonical);
    }

    /* ---------- digit boundary ---------- */
    #[test] fn digit_boundary() {
        let a = uidb!("GL3DModel");
        assert_eq!(a.as_id().normalized, vec![
            NormalizedPart::Str("gl"),
            NormalizedPart::Str("3d"),
            NormalizedPart::Str("model"),
        ]);
    }
}
