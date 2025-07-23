use super::{
	ClosedOverReferencesInScope, Context, ContextId, ContextType, Environment, GeneralContext,
	LocalInformation,
};
use crate::{
	features::{
		modules::{Exported, SynthesisedModule},
		variables::VariableOrImport,
	},
	types::TypeId,
	CheckingData,
};
use source_map::SourceId;
use std::{collections::HashMap, iter::FromIterator, mem};
use unified_identifier::UnifiedIdentifierBuf;

pub type RootContext = Context<Root>;

#[derive(Debug)]
pub struct Root;

impl ContextType for Root {
	fn as_general_context(et: &Context<Self>) -> GeneralContext<'_> {
		GeneralContext::Root(et)
	}

	fn get_parent(&self) -> Option<&GeneralContext<'_>> {
		None
	}

	fn as_syntax(&self) -> Option<&super::Syntax> {
		None
	}

	fn get_closed_over_references_mut(&mut self) -> Option<&mut ClosedOverReferencesInScope> {
		None
	}
}

impl RootContext {
	/// Merges two [`RootEnvironments`]. May be used for multiple `.d.ts` files
	pub(crate) fn _union(&mut self, other: Self) {
		// TODO this is bad, some things need to merge, inserting over existing will be bad
		self.variables.extend(other.variables);
		todo!()
		// self.tys.extend(other.tys.into_iter());
	}

	/// **For testing only**
	#[must_use]
	#[allow(clippy::needless_lifetimes)]
	pub fn new_testing_context<'a>(&'a self) -> Environment<'a> {
		self.new_lexical_environment(crate::Scope::Block {})
	}

	#[must_use]
	pub fn new_with_primitive_references() -> Self {
		// TODO number might not be a reference at some point
                let named_types = HashMap::from_iter([
                        (UnifiedIdentifierBuf::new("number"), TypeId::NUMBER_TYPE),
                        (UnifiedIdentifierBuf::new("string"), TypeId::STRING_TYPE),
                        (UnifiedIdentifierBuf::new("boolean"), TypeId::BOOLEAN_TYPE),
                        (UnifiedIdentifierBuf::new("null"), TypeId::NULL_TYPE),
                        (UnifiedIdentifierBuf::new("undefined"), TypeId::UNDEFINED_TYPE),
                        (UnifiedIdentifierBuf::new("void"), TypeId::VOID_TYPE),
                        (UnifiedIdentifierBuf::new("Array"), TypeId::ARRAY_TYPE),
                        (UnifiedIdentifierBuf::new("Promise"), TypeId::PROMISE_TYPE),
                        (UnifiedIdentifierBuf::new("RegExp"), TypeId::REGEXP_TYPE),
                        (UnifiedIdentifierBuf::new("ImportMeta"), TypeId::IMPORT_META),
                        (UnifiedIdentifierBuf::new("Function"), TypeId::FUNCTION_TYPE),
                        (UnifiedIdentifierBuf::new("object"), TypeId::OBJECT_TYPE),
                        (UnifiedIdentifierBuf::new("Literal"), TypeId::LITERAL_RESTRICTION),
                        (UnifiedIdentifierBuf::new("Readonly"), TypeId::READONLY_RESTRICTION),
                        (UnifiedIdentifierBuf::new("Exclusive"), TypeId::EXCLUSIVE_RESTRICTION),
                        (UnifiedIdentifierBuf::new("Uppercase"), TypeId::STRING_UPPERCASE),
                        (UnifiedIdentifierBuf::new("Lowercase"), TypeId::STRING_LOWERCASE),
                        (UnifiedIdentifierBuf::new("Capitalize"), TypeId::STRING_CAPITALIZE),
                        (UnifiedIdentifierBuf::new("Uncapitalize"), TypeId::STRING_UNCAPITALIZE),
                        (UnifiedIdentifierBuf::new("NoInfer"), TypeId::NO_INFER),
                        (UnifiedIdentifierBuf::new("GreaterThan"), TypeId::GREATER_THAN),
                        (UnifiedIdentifierBuf::new("LessThan"), TypeId::LESS_THAN),
                        (UnifiedIdentifierBuf::new("MultipleOf"), TypeId::MULTIPLE_OF),
                        (UnifiedIdentifierBuf::new("NotNotANumber"), TypeId::NUMBER_BUT_NOT_NOT_A_NUMBER),
                        (UnifiedIdentifierBuf::new("Not"), TypeId::NOT_RESTRICTION),
                        (UnifiedIdentifierBuf::new("CaseInsensitive"), TypeId::CASE_INSENSITIVE),
                        (UnifiedIdentifierBuf::new("Infinity"), TypeId::INFINITY),
                        (UnifiedIdentifierBuf::new("NegativeInfinity"), TypeId::NEG_INFINITY),
                        (UnifiedIdentifierBuf::new("MinFloat"), TypeId::FLOAT_MIN),
                        (UnifiedIdentifierBuf::new("MaxFloat"), TypeId::FLOAT_MAX),
                ]);

		let mut info = LocalInformation::default();

		// Add undefined as a variable
		let variables = {
			let variable_or_import = VariableOrImport::Variable {
				mutability: crate::features::variables::VariableMutability::Constant,
				declared_at: source_map::Nullable::NULL,
				context: None,
				allow_reregistration: false,
			};
			let undefined_id = variable_or_import.get_id();
                        let variables = [(UnifiedIdentifierBuf::new("undefined"), variable_or_import)];
			info.variable_current_value.insert(undefined_id, TypeId::UNDEFINED_TYPE);
			variables
		};

		Self {
			context_type: Root,
			context_id: ContextId::ROOT,
			named_types,
			variables: HashMap::from_iter(variables),
			variable_names: Default::default(),
			deferred_function_constraints: Default::default(),
			// TODO
			can_reference_this: crate::context::CanReferenceThis::Yeah,
			info,
			possibly_mutated_objects: Default::default(),
			possibly_mutated_variables: Default::default(),
		}
	}

	pub fn new_module_context<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
		&self,
		source: SourceId,
		module: A::Module<'static>,
		checking_data: &'a mut CheckingData<T, A>,
	) -> &'a SynthesisedModule<A::OwnedModule> {
		let module_scope = crate::Scope::Module { source, exported: Exported::default() };
		let mut environment = self.new_lexical_environment(module_scope);
		A::synthesise_module(&module, source, &mut environment, checking_data);

		let crate::Scope::Module { exported, .. } = environment.context_type.scope else {
			unreachable!()
		};

		let module = SynthesisedModule {
			content: A::owned_module_from_module(module),
			exported,
			info: environment.info,
			// TODO temp
			mappings: mem::take(&mut checking_data.local_type_mappings),
		};

		// TODO better way to do this?
		checking_data.modules.synthesised_modules.insert(source, module);
		checking_data.modules.synthesised_modules.get(&source).unwrap()
	}

	/// TODO working things out:
	/// - strings could reference a big string
	#[must_use]
	pub fn serialize(self) -> Vec<u8> {
		todo!()
	}

	pub fn deserialize(_source: &[u8], _backing_source: SourceId) -> Result<Self, String> {
		todo!()
		// let mut ctx = Root::new_with_primitive_references();

		// if !source.starts_with(HEADER) {
		// 	return Err("Missing header".to_owned());
		// }

		// let mut bytes = source.into_iter();

		// {
		// 	assert_eq!(bytes.by_ref().take(HEADER.len()).collect::<Vec<_>>(), HEADER);
		// }

		// // Types
		// let count = u16::from_le_bytes([bytes.next().unwrap(), bytes.next().unwrap()]);

		// for _ in 0..count {
		// 	let ty = Type::deserialize(&mut bytes, backing_source);
		// 	ctx.new_type(ty);
		// }
		// crate::utilities::notify!("Registered {:?} types", count);

		// ctx.variables = BinarySerializable::deserialize(&mut bytes, backing_source);
		// // TODO terrible
		// VariableId::set_counter_bad((ctx.variables.len() + 1) as u16);
		// ctx.variable_names = BinarySerializable::deserialize(&mut bytes, backing_source);

		// ctx.proofs = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.functions_on_type = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.subtyping_constant_proofs = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.terms_reverse = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.proxies = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.can_reference_this = BinarySerializable::deserialize(&mut bytes, backing_source);

		// Ok(ctx)
	}
}
