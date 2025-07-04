use crate::{ASTNode, Expression, PropertyReference, Statement, VariableIdentifier};

/// A trait which means that self can be pushed to a [`TokenSender`]
pub trait IntoAST<T> {
	fn into_ast(self) -> T;
}

impl<T: ASTNode> IntoAST<T> for T {
	fn into_ast(self) -> T {
		self
	}
}

pub struct Ident<'a>(&'a str);

impl<'a> From<&'a str> for Ident<'a> {
	fn from(name: &'a str) -> Self {
		Self(name)
	}
}

impl IntoAST<Expression> for Ident<'_> {
	fn into_ast(self) -> Expression {
		Expression::VariableReference(self.0.to_owned(), source_map::Nullable::NULL)
	}
}

impl IntoAST<Expression> for &str {
	fn into_ast(self) -> Expression {
		Expression::StringLiteral(
			self.to_owned(),
			crate::Quoted::Double,
			source_map::Nullable::NULL,
		)
	}
}

impl IntoAST<PropertyReference> for &str {
	fn into_ast(self) -> PropertyReference {
		PropertyReference::Standard { property: self.to_owned(), is_private: false }
	}
}

impl IntoAST<VariableIdentifier> for &str {
	fn into_ast(self) -> VariableIdentifier {
		VariableIdentifier::Standard(self.into(), source_map::Nullable::NULL)
	}
}

#[allow(clippy::cast_precision_loss)]
impl IntoAST<Expression> for usize {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(
			crate::number::NumberRepresentation::from(self as f64),
			source_map::Nullable::NULL,
		)
	}
}

impl IntoAST<Expression> for f64 {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(
			crate::number::NumberRepresentation::from(self),
			source_map::Nullable::NULL,
		)
	}
}

impl IntoAST<Statement> for Expression {
	fn into_ast(self) -> Statement {
		Statement::Expression(self.into())
	}
}
