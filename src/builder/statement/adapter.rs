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

///Adapter for receiving the `do ...` statement in a `do ... while(...)` loop.
pub struct DoAdapter<N: Build<ast::Statement>>(N);

impl<N: Build<ast::Statement>> From<N> for DoAdapter<N> {
    fn from(next: N) -> DoAdapter<N> {
        DoAdapter(next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::Statement> for DoAdapter<N> {
    type Next = ExpressionBuilder<DoWhileAdapter<N>>;

    fn build_with(self, result: ast::Statement) -> ExpressionBuilder<DoWhileAdapter<N>> {
        ExpressionBuilder::from(DoWhileAdapter(result, self.0))
    }
}

///Adapter for receiving the `while(...)` condition in a `do ... while(...)` loop.
pub struct DoWhileAdapter<N: Build<ast::Statement>>(ast::Statement, N);

impl<N: Build<ast::Statement>> Build<ast::Expression> for DoWhileAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Expression) -> N::Next {
        self.build_with(ast::Expressions { exprs: vec![result] })
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for DoWhileAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Expressions) -> N::Next {
        self.1.build_with(ast::Statement::Do(Box::new(self.0), result))
    }
}

///Adapter for receiving the `while(...)` condition in a `while(...) ...` loop.
pub struct WhileAdapter<N: Build<ast::Statement>>(N);

impl<N: Build<ast::Statement>> From<N> for WhileAdapter<N> {
    fn from(next: N) -> WhileAdapter<N> {
        WhileAdapter(next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expression> for WhileAdapter<N> {
    type Next = StatementBuilder<WhileDoAdapter<N>>;

    fn build_with(self, result: ast::Expression) -> StatementBuilder<WhileDoAdapter<N>> {
        self.build_with(ast::Expressions { exprs: vec![result] })
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for WhileAdapter<N> {
    type Next = StatementBuilder<WhileDoAdapter<N>>;

    fn build_with(self, result: ast::Expressions) -> StatementBuilder<WhileDoAdapter<N>> {
        StatementBuilder::from(WhileDoAdapter(result, self.0))
    }
}

///Adapter for receiving the loop body in a `while(...) ...` loop.
pub struct WhileDoAdapter<N: Build<ast::Statement>>(ast::Expressions, N);

impl<N: Build<ast::Statement>> Build<ast::Statement> for WhileDoAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Statement) -> N::Next {
        self.1.build_with(ast::Statement::While(self.0, Box::new(result)))
    }
}

///Adapter for receiving the initiation statement of a `for` loop.
pub struct ForInitAdapter<N: Build<ast::Statement>>(N);

impl<N: Build<ast::Statement>> From<N> for ForInitAdapter<N> {
    fn from(next: N) -> ForInitAdapter<N> {
        ForInitAdapter(next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expression> for ForInitAdapter<N> {
    type Next = ExpressionBuilder<ForCondAdapter<N>>;

    fn build_with(self, result: ast::Expression) -> ExpressionBuilder<ForCondAdapter<N>> {
        self.build_with(ast::Expressions { exprs: vec![result] })
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for ForInitAdapter<N> {
    type Next = ExpressionBuilder<ForCondAdapter<N>>;

    fn build_with(self, result: ast::Expressions) -> ExpressionBuilder<ForCondAdapter<N>> {
        ExpressionBuilder::from(ForCondAdapter(ForKind::Regular(result), self.0))
    }
}

impl<N: Build<ast::Statement>> Build<Vec<ast::VarDecl>> for ForInitAdapter<N> {
    type Next = ExpressionBuilder<ForCondAdapter<N>>;

    fn build_with(self, result: Vec<ast::VarDecl>) -> ExpressionBuilder<ForCondAdapter<N>> {
        ExpressionBuilder::from(ForCondAdapter(ForKind::Var(result), self.0))
    }
}

enum ForKind {
    Regular(ast::Expressions),
    Var(Vec<ast::VarDecl>),
}

///Adapter for receiving the condition expression of a `for` loop.
pub struct ForCondAdapter<N: Build<ast::Statement>>(ForKind, N);

impl<N: Build<ast::Statement>> Build<ast::Expression> for ForCondAdapter<N> {
    type Next = ExpressionBuilder<ForIncrAdapter<N>>;

    fn build_with(self, result: ast::Expression) -> ExpressionBuilder<ForIncrAdapter<N>> {
        self.build_with(ast::Expressions { exprs: vec![result] })
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for ForCondAdapter<N> {
    type Next = ExpressionBuilder<ForIncrAdapter<N>>;

    fn build_with(self, result: ast::Expressions) -> ExpressionBuilder<ForIncrAdapter<N>> {
        ExpressionBuilder::from(ForIncrAdapter(self.0, result, self.1))
    }
}

///Adapter for receiving the incrementation expression of a `for` loop.
pub struct ForIncrAdapter<N: Build<ast::Statement>>(ForKind, ast::Expressions, N);

impl<N: Build<ast::Statement>> Build<ast::Expression> for ForIncrAdapter<N> {
    type Next = StatementBuilder<ForStmtAdapter<N>>;

    fn build_with(self, result: ast::Expression) -> StatementBuilder<ForStmtAdapter<N>> {
        self.build_with(ast::Expressions { exprs: vec![result] })
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for ForIncrAdapter<N> {
    type Next = StatementBuilder<ForStmtAdapter<N>>;

    fn build_with(self, result: ast::Expressions) -> StatementBuilder<ForStmtAdapter<N>> {
        StatementBuilder::from(ForStmtAdapter(self.0, self.1, result, self.2))
    }
}

///Adapter for receiving the body statement of a `for` loop.
pub struct ForStmtAdapter<N: Build<ast::Statement>>(ForKind, ast::Expressions, ast::Expressions, N);

impl<N: Build<ast::Statement>> Build<ast::Statement> for ForStmtAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Statement) -> N::Next {
        match self.0 {
            ForKind::Regular(init) => self.3.build_with(ast::Statement::For(init, self.1, self.2, Box::new(result))),
            ForKind::Var(init) => self.3.build_with(ast::Statement::ForVar(init, self.1, self.2, Box::new(result))),
        }
    }
}
