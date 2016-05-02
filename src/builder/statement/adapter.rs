//!Adapters for transitioning between builders.

use ast;
use builder::Build;
use builder::expression::ExpressionBuilder;
use super::{IfBuilder, StatementBuilder, VarsBuilder};

///An adapter that holds the name of a variable.
pub struct InitAdapter<N: Build<Vec<ast::VarDecl>>>(ast::Identifier, VarsBuilder<N>);

impl<N: Build<Vec<ast::VarDecl>>> InitAdapter<N> {
    ///Create a variable initializer adapter from a variable name and a
    ///builder.
    pub fn from<S: Into<ast::Identifier>>(id: S, next: VarsBuilder<N>) -> InitAdapter<N> {
        InitAdapter(id.into(), next)
    }
}

impl<N: Build<Vec<ast::VarDecl>>> Build<ast::Expression> for InitAdapter<N> {
    type Next = VarsBuilder<N>;
    fn build_with(self, result: ast::Expression) -> VarsBuilder<N> {
        let InitAdapter(id, mut vars) = self;
        vars.0.push(ast::VarDecl {
            ident: id,
            init: Some(result),
        });
        vars
    }
}

///An adapter for the condition expression in an `if` statement.
pub struct IfAdapter<N>(N);

impl<N: Build<ast::Statement>> From<N> for IfAdapter<N> {
    fn from(next: N) -> IfAdapter<N> {
        IfAdapter(next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expression> for IfAdapter<N> {
    type Next = StatementBuilder<ThenAdapter<N>>;
    fn build_with(self, result: ast::Expression) -> StatementBuilder<ThenAdapter<N>> {
        self.build_with(ast::Expressions::from(vec![result]))
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for IfAdapter<N> {
    type Next = StatementBuilder<ThenAdapter<N>>;
    fn build_with(self, result: ast::Expressions) -> StatementBuilder<ThenAdapter<N>> {
        StatementBuilder(ThenAdapter {
            condition: result,
            next: self.0,
        })
    }
}

///An adapter for holding the `if` condition while the first branch is built.
pub struct ThenAdapter<N> {
    condition: ast::Expressions,
    next: N,
}

impl<N: Build<ast::Statement>> Build<ast::Statement> for ThenAdapter<N> {
    type Next = IfBuilder<N>;
    fn build_with(self, result: ast::Statement) -> IfBuilder<N> {
        IfBuilder {
            condition: self.condition,
            then: result,
            next: self.next,
        }
    }
}

enum ForInLhs {
    Var(ast::Identifier),
    Expr(ast::LhsExpression),
}

///An adapter for the left hand side of a `for(... in ...)` statement.
pub struct ForInLhsAdapter<N: Build<ast::Statement>>(N);

impl<N: Build<ast::Statement>> From<N> for ForInLhsAdapter<N> {
    fn from(next: N) -> ForInLhsAdapter<N> {
        ForInLhsAdapter(next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::LhsExpression> for ForInLhsAdapter<N> {
    type Next = ExpressionBuilder<ForInRhsAdapter<N>>;

    fn build_with(self, result: ast::LhsExpression) -> ExpressionBuilder<ForInRhsAdapter<N>> {
        ExpressionBuilder::from(ForInRhsAdapter(ForInLhs::Expr(result), self.0))
    }
}

///An adapter for the right hand side of a `for(... in ...)` statement.
pub struct ForInRhsAdapter<N: Build<ast::Statement>>(ForInLhs, N);

impl<N: Build<ast::Statement>> ForInRhsAdapter<N> {
    ///Create a `for(var ... in ...)` RHS expression adapter from a variable
    ///name and a builder.
    pub fn from<S: Into<ast::Identifier>>(id: S, next: N) -> ForInRhsAdapter<N> {
        ForInRhsAdapter(ForInLhs::Var(id.into()), next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expression> for ForInRhsAdapter<N> {
    type Next = StatementBuilder<ForInStmtAdapter<N>>;

    fn build_with(self, result: ast::Expression) -> StatementBuilder<ForInStmtAdapter<N>> {
        self.build_with(ast::Expressions {
            exprs: vec![result]
        })
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for ForInRhsAdapter<N> {
    type Next = StatementBuilder<ForInStmtAdapter<N>>;

    fn build_with(self, result: ast::Expressions) -> StatementBuilder<ForInStmtAdapter<N>> {
        StatementBuilder(ForInStmtAdapter(self.0, result, self.1))
    }
}

///An adapter for the loop body of `for(... in ...)` loops.
pub struct ForInStmtAdapter<N: Build<ast::Statement>>(ForInLhs, ast::Expressions, N);

impl<N: Build<ast::Statement>> Build<ast::Statement> for ForInStmtAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Statement) -> N::Next {
        match self.0 {
            ForInLhs::Expr(lhs) => self.2.build_with(ast::Statement::ForIn(lhs, self.1, Box::new(result))),
            ForInLhs::Var(var) => self.2.build_with(ast::Statement::ForInVar(var, self.1, Box::new(result))),
        }
    }
}

///An expression adapter for `return` statements.
pub struct ReturnAdapter<N: Build<ast::Statement>>(N);

impl<N: Build<ast::Statement>> From<N> for ReturnAdapter<N> {
    fn from(next: N) -> ReturnAdapter<N> {
        ReturnAdapter(next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for ReturnAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Expressions) -> N::Next {
        self.0.build_with(ast::Statement::Return(result))
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expression> for ReturnAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Expression) -> N::Next {
        self.build_with(ast::Expressions { exprs: vec![result] })
    }
}
