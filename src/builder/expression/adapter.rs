//!Adapters for transitioning between builders.

use builder::Build;
use builder::expression::{ExpressionBuilder, PathType, PathBuilder, BinaryExprBuilder};
use builder::function::{FnBodyBuilder, FnCall, FnCallBuilder};
use ast;

///An adapter that receives the name of an object field (property).
pub struct PropAssignAdapter<N: Build<ast::PropertyAssignment>>(N);

impl<N: Build<ast::PropertyAssignment>> From<N> for PropAssignAdapter<N> {
    fn from(next: N) -> PropAssignAdapter<N> {
        PropAssignAdapter(next)
    }
}

impl<N: Build<ast::PropertyAssignment>> Build<ast::PropertyName> for PropAssignAdapter<N> {
    type Next = ExpressionBuilder<PropInitAdapter<N>>;

    fn build_with(self, result: ast::PropertyName) -> ExpressionBuilder<PropInitAdapter<N>> {
        ExpressionBuilder::from(PropInitAdapter(result, self.0))
    }
}

///An adapter that holds the name of an object field (property).
pub struct PropInitAdapter<N: Build<ast::PropertyAssignment>>(ast::PropertyName, N);

impl<N: Build<ast::PropertyAssignment>> Build<ast::Expression> for PropInitAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Expression) -> N::Next {
        self.1.build_with(ast::PropertyAssignment::Assignment(self.0, result))
    }
}

///An adapter that receives the name of an object field getter.
pub struct GetterAdapter<N: Build<ast::PropertyAssignment>>(N);

impl<N: Build<ast::PropertyAssignment>> From<N> for GetterAdapter<N> {
    fn from(next: N) -> GetterAdapter<N> {
        GetterAdapter(next)
    }
}

impl<N: Build<ast::PropertyAssignment>> Build<ast::PropertyName> for GetterAdapter<N> {
    type Next = FnBodyBuilder<GetterBodyAdapter<N>>;

    fn build_with(self, result: ast::PropertyName) -> FnBodyBuilder<GetterBodyAdapter<N>> {
        FnBodyBuilder::from(GetterBodyAdapter(result, self.0))
    }
}

///An adapter that holds the name of an object field getter.
pub struct GetterBodyAdapter<N: Build<ast::PropertyAssignment>>(ast::PropertyName, N);

impl<N: Build<ast::PropertyAssignment>> Build<ast::FunctionBody> for GetterBodyAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::FunctionBody) -> N::Next {
        self.1.build_with(ast::PropertyAssignment::Getter(self.0, result))
    }
}

///An adapter that holds the argument, and receives the name of an object field setter.
pub struct SetterAdapter<N: Build<ast::PropertyAssignment>>(ast::Identifier, N);

impl<N: Build<ast::PropertyAssignment>> SetterAdapter<N> {
    ///Create a new object field setter adapter from an argument name and a
    ///builder.
    pub fn from<S: Into<ast::Identifier>>(id: S, next: N) -> SetterAdapter<N> {
        SetterAdapter(id.into(), next)
    }
}

impl<N: Build<ast::PropertyAssignment>> Build<ast::PropertyName> for SetterAdapter<N> {
    type Next = FnBodyBuilder<SetterBodyAdapter<N>>;

    fn build_with(self, result: ast::PropertyName) -> FnBodyBuilder<SetterBodyAdapter<N>> {
        FnBodyBuilder::from(SetterBodyAdapter(result, self.0, self.1))
    }
}


///An adapter that holds the argument and the name of an object field setter.
pub struct SetterBodyAdapter<N: Build<ast::PropertyAssignment>>(ast::PropertyName, ast::Identifier, N);

impl<N: Build<ast::PropertyAssignment>> Build<ast::FunctionBody> for SetterBodyAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::FunctionBody) -> N::Next {
        self.2.build_with(ast::PropertyAssignment::Setter(self.0, self.1, result))
    }
}

///An adapter for function call expressions.
pub struct CallAdapter<N: Build<ast::CallExpression>>(N);

impl<N: Build<ast::CallExpression>> From<N> for CallAdapter<N> {
    fn from(next: N) -> CallAdapter<N> {
        CallAdapter(next)
    }
}

impl<E: PathType, N: Build<ast::CallExpression>> Build<FnCall<E>> for CallAdapter<N> {
    type Next = PathBuilder<ast::CallExpression, N>;

    fn build_with(self, result: FnCall<E>) -> PathBuilder<ast::CallExpression, N> {
        PathBuilder(
            ast::CallExpression {
                base: result.access_expr.into_call_base(),
                arguments: result.args,
                access: vec![],
            },
            self.0,
        )
    }
}

///An adapter that holds the assignment expression type.
pub struct AssignAdapter<N: Build<ast::Expression>>(Option<ast::AssignmentOp>, N);

impl<N: Build<ast::Expression>> AssignAdapter<N> {
    ///Create a new assignment expression adapter from an operator and a builder.
    pub fn with_op(op: ast::AssignmentOp, next: N) -> AssignAdapter<N> {
        AssignAdapter(Some(op), next)
    }
}

impl<N: Build<ast::Expression>> From<N> for AssignAdapter<N> {
    fn from(next: N) -> AssignAdapter<N> {
        AssignAdapter(None, next)
    }
}

impl<N: Build<ast::Expression>> Build<ast::LhsExpression> for AssignAdapter<N> {
    type Next = ExpressionBuilder<AssignRhsAdapter<N>>;

    fn build_with(self, result: ast::LhsExpression) -> ExpressionBuilder<AssignRhsAdapter<N>> {
        ExpressionBuilder(AssignRhsAdapter(result, self.0, self.1))
    }
}

///An adapter that holds the type and left hand side of an assignment expression.
pub struct AssignRhsAdapter<N: Build<ast::Expression>>(ast::LhsExpression, Option<ast::AssignmentOp>, N);

impl<N: Build<ast::Expression>> Build<ast::Expression> for AssignRhsAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Expression) -> N::Next {
        self.2.build_with(ast::Expression::Assignment(self.0, self.1, Box::new(result)))
    }
}

///An adapter that holds the type of a binary operator.
pub struct BinaryLhsAdapter<N: Build<ast::Expression>>(ast::BinaryOp, N);

impl<N: Build<ast::Expression>> BinaryLhsAdapter<N> {
    ///Create a new binary operator adapter from an operator and a builder.
    pub fn from(op: ast::BinaryOp, next: N) -> BinaryLhsAdapter<N> {
        BinaryLhsAdapter(op, next)
    }
}

impl<N: Build<ast::Expression>> Build<ast::Expression> for BinaryLhsAdapter<N> {
    type Next = ExpressionBuilder<BinaryExprBuilder<N>>;

    fn build_with(self, result: ast::Expression) -> ExpressionBuilder<BinaryExprBuilder<N>> {
        ExpressionBuilder(BinaryExprBuilder(self.0, vec![result], self.1))
    }
}

///An adapter that holds the type of a unary operator.
pub struct UnaryAdapter<N: Build<ast::Expression>>(ast::UnaryOp, N);

impl<N: Build<ast::Expression>> UnaryAdapter<N> {
    ///Create a new unary operator adapter from an operator and a builder.
    pub fn from(op: ast::UnaryOp, next: N) -> UnaryAdapter<N> {
        UnaryAdapter(op, next)
    }
}

impl<N: Build<ast::Expression>> Build<ast::LhsExpression> for UnaryAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::LhsExpression) -> N::Next {
        self.1.build_with(ast::Expression::Unary(self.0, result))
    }
}

///An adapter that receives the path to a function.
pub struct FnAccessAdapter<N: Build<FnCall<ast::PathExpression>>>(N);

impl<N: Build<FnCall<ast::PathExpression>>> From<N> for FnAccessAdapter<N> {
    fn from(next: N) -> FnAccessAdapter<N> {
        FnAccessAdapter(next)
    }
}

impl<N: Build<FnCall<ast::PathExpression>>> Build<ast::PathExpression> for FnAccessAdapter<N> {
    type Next = FnCallBuilder<ast::PathExpression, N>;

    fn build_with(self, result: ast::PathExpression) -> FnCallBuilder<ast::PathExpression, N> {
        FnCallBuilder::from(result, self.0)
    }
}

///The parts of a function expression.
pub struct FnExprDecl {
    ///The function name.
    pub name: Option<ast::Identifier>,

    ///The function argument names.
    pub args: Vec<ast::Identifier>,

    ///The function body.
    pub body: ast::FunctionBody,
}

///An adapter that holds parts of a function expression.
pub struct FnExprAdapter<N: Build<FnExprDecl>> {
    name: Option<ast::Identifier>,
    args: Vec<ast::Identifier>,
    next: N
}

impl<N: Build<FnExprDecl>> FnExprAdapter<N> {
    ///Create a new function expression adapter with a function name.
    pub fn with_name<S: Into<ast::Identifier>>(id: S, next: N) -> FnExprAdapter<N> {
        FnExprAdapter {
            name: Some(id.into()),
            args: vec![],
            next: next,
        }
    }
}

impl<N: Build<FnExprDecl>> From<N> for FnExprAdapter<N> {
    fn from(next: N) -> FnExprAdapter<N> {
        FnExprAdapter {
            name: None,
            args: vec![],
            next: next,
        }
    }
}

impl<N: Build<FnExprDecl>> Build<Vec<ast::Identifier>> for FnExprAdapter<N> {
    type Next = FnBodyBuilder<Self>;

    fn build_with(mut self, result: Vec<ast::Identifier>) -> FnBodyBuilder<Self> {
        self.args = result;
        FnBodyBuilder::from(self)
    }
}

impl<N: Build<FnExprDecl>> Build<ast::FunctionBody> for FnExprAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::FunctionBody) -> N::Next {
        self.next.build_with(FnExprDecl {
            name: self.name,
            args: self.args,
            body: result,
        })
    }
}
