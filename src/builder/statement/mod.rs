//!Statement related builders.

use builder::Build;
use builder::expression::{ExpressionBuilder, LhsExprBuilder};
use builder::function::{FnAdapter, FnArgnamesBuilder};
use ast;

use self::adapter::*;

pub mod adapter;


#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for statement blocks.
pub struct BlockBuilder<N: Build<ast::Block>>(ast::Block, N);

impl<N: Build<ast::Block>> BlockBuilder<N> {
    ///Build a new statement and add it to the block.
    ///
    ///```
    ///use js_builder::ast::Block;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Block::build()
    ///    .stmt().var().inited("a").id("b").build()
    ///    .build() //finish building the block
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{var a = b;}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn stmt(self) -> StatementBuilder<Self> {
        StatementBuilder(self)
    }

    ///Same as `stmt`, above, but doesn't move the builder.
    pub fn add_stmt(&mut self) -> StatementBuilder<&mut Self> {
        StatementBuilder(self)
    }

    ///Add a pre-built statement to the block.
    pub fn with_stmt(&mut self, stmt: ast::Statement) -> &mut BlockBuilder<N> {
        self.0.statements.push(stmt);
        self
    }

    ///Finish building this statement block.
    pub fn build(self) -> N::Next {
        self.1.build_with(self.0)
    }

    ///Use a pre-built statement block, instead.
    pub fn build_block(self, block: ast::Block) -> N::Next {
        self.1.build_with(block)
    }
}

impl<N: Build<ast::Block>> From<N> for BlockBuilder<N> {
    fn from(next: N) -> BlockBuilder<N> {
        BlockBuilder(ast::Block {
            statements: vec![]
        }, next)
    }
}

impl<N: Build<ast::Block>> Build<ast::Statement> for BlockBuilder<N> {
    type Next = BlockBuilder<N>;
    fn build_with(mut self, result: ast::Statement) -> BlockBuilder<N> {
        self.0.statements.push(result);
        self
    }
}

impl<'a, N: Build<ast::Block>> Build<ast::Statement> for &'a mut BlockBuilder<N> {
    type Next = &'a mut BlockBuilder<N>;
    fn build_with(mut self, result: ast::Statement) -> &'a mut BlockBuilder<N> {
        self.0.statements.push(result);
        self
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for statements.
pub struct StatementBuilder<N: Build<ast::Statement>>(N);

impl<N: Build<ast::Statement>> StatementBuilder<N> {
    ///Build a statement block statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().block()
    ///    .stmt().var().inited("a").id("b").build()
    ///    .build() //finish building the block
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{var a = b;}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn block(self) -> BlockBuilder<Self> {
        BlockBuilder::from(self)
    }

    ///Build a variable declaration statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().var()
    ///    .inited("a").id("b") //a = b
    ///    .uninited("c") //declare, but don't assign to c
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "var a = b, c;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn var(self) -> VarsBuilder<Self> {
        VarsBuilder::from(self)
    }

    ///Build an empty statement. It's just a `;`.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().empty().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    ";",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn empty(self) -> N::Next {
        self.0.build_with(ast::Statement::Empty)
    }

    ///Build an expression statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().expr().assign()
    ///    .id("a").add()
    ///        .id("b")
    ///        .id("c")
    ///        .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a = b + c;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn expr(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder::from(self)
    }

    ///Build an `if` statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().if_().eq().id("a").id("b").build() //condition expression
    ///        .expr().assign().id("a").add().id("b").id("c").build() //if-true-statement
    ///    .build() //no else this time
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "if(a == b) a = b + c;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn if_(self) -> ExpressionBuilder<IfAdapter<N>> {
        ExpressionBuilder::from(IfAdapter::from(self.0))
    }

    ///Build a `for(... in ...)` statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().for_in().id("a").id("c")
    ///    .expr().add_assign().id("total").path().id("c").index().id("a").build() //loop body
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "for(a in c) total += c[a];",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn for_in(self) -> LhsExprBuilder<ForInLhsAdapter<N>> {
        LhsExprBuilder::from(ForInLhsAdapter::from(self.0))
    }

    ///Build a `for(var ... in ...)` statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().for_in_var("a").id("c")
    ///    .expr().add_assign().id("total").path().id("c").index().id("a").build() //loop body
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "for(var a in c) total += c[a];",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn for_in_var<S: Into<ast::Identifier>>(self, var: S) -> ExpressionBuilder<ForInRhsAdapter<N>> {
        ExpressionBuilder::from(ForInRhsAdapter::from(var, self.0))
    }

    ///Build a `return` statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().return_().add()
    ///    .id("a")
    ///    .id("b")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "return a + b;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn return_(self) -> ExpressionBuilder<ReturnAdapter<N>> {
        ExpressionBuilder::from(ReturnAdapter::from(self.0))
    }

    ///Build an empty `return` statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().return_nothing().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "return;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn return_nothing(self) -> N::Next {
        self.return_().comma().build()
    }

    ///Use a pre-built statement, instead.
    pub fn build_stmt(self, stmt: ast::Statement) -> N::Next {
        self.0.build_with(stmt)
    }
}

impl<N: Build<ast::Statement>> From<N> for StatementBuilder<N> {
    fn from(next: N) -> StatementBuilder<N> {
        StatementBuilder(next)
    }
}

impl<N: Build<ast::Statement>> Build<ast::Block> for StatementBuilder<N> {
    type Next = N::Next;
    fn build_with(self, result: ast::Block) -> N::Next {
        self.0.build_with(ast::Statement::Block(result))
    }
}

impl<N: Build<ast::Statement>> Build<Vec<ast::VarDecl>> for StatementBuilder<N> {
    type Next = N::Next;
    fn build_with(self, result: Vec<ast::VarDecl>) -> N::Next {
        self.0.build_with(ast::Statement::Variable(result))
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expression> for StatementBuilder<N> {
    type Next = N::Next;
    fn build_with(self, result: ast::Expression) -> N::Next {
        self.build_with(ast::Expressions::from(vec![result]))
    }
}

impl<N: Build<ast::Statement>> Build<ast::Expressions> for StatementBuilder<N> {
    type Next = N::Next;
    fn build_with(self, result: ast::Expressions) -> N::Next {
        self.0.build_with(ast::Statement::Expression(result))
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for variable declaration statements.
pub struct VarsBuilder<N: Build<Vec<ast::VarDecl>>>(Vec<ast::VarDecl>, N);

impl<N: Build<Vec<ast::VarDecl>>> VarsBuilder<N> {
    ///Add an initialized variable to the variable declaration statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().var()
    ///    .inited("a").id("b")
    ///    .build() //no more variables
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "var a = b;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn inited<S: Into<ast::Identifier>>(self, id: S) -> ExpressionBuilder<InitAdapter<N>> {
        ExpressionBuilder::from(InitAdapter::from(id, self))
    }

    ///Add an uninitialized variable to the variable declaration statement.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().var()
    ///    .uninited("a")
    ///    .build() //no more variables
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "var a;",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn uninited<S: Into<ast::Identifier>>(mut self, id: S) -> VarsBuilder<N> {
        self.0.push(ast::VarDecl {
            ident: id.into(),
            init: None,
        });
        self
    }

    ///Finish declaring variables.
    pub fn build(self) -> N::Next {
        self.1.build_with(self.0)
    }
}

impl<N: Build<Vec<ast::VarDecl>>> From<N> for VarsBuilder<N> {
    fn from(next: N) -> VarsBuilder<N> {
        VarsBuilder(vec![], next)
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for `if` statements.
pub struct IfBuilder<N: Build<ast::Statement>> {
    condition: ast::Expressions,
    then: ast::Statement,
    next: N,
}

impl<N: Build<ast::Statement>> IfBuilder<N> {
    ///Add an `else` branch.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().if_().eq().id("a").id("b").build() //condition expression
    ///        .expr().add_assign().id("a").id("b") //if-true-statement
    ///    .else_()
    ///        .expr().sub_assign().id("a").id("b") //else-statement
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "if(a == b) a += b;else a -= b;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn else_(self) -> StatementBuilder<Self> {
        StatementBuilder(self)
    }

    ///Add an `else if` branch.
    ///
    ///```
    ///use js_builder::ast::Statement;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Statement::build().if_().eq().id("a").id("b").build() //condition expression
    ///        .expr().add_assign().id("a").id("b") //if-true-statement
    ///    .else_if().gt().id("a").id("b").build()
    ///        .expr().sub_assign().id("a").id("b") //else-if-statement
    ///    .build() //no else this time
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "if(a == b) a += b;else if(a > b) a -= b;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn else_if(self) -> ExpressionBuilder<IfAdapter<Self>> {
        self.else_().if_()
    }

    ///Build the `if` statement without an `else` branch.
    pub fn build(self) -> N::Next {
        self.next.build_with(ast::Statement::If(self.condition, Box::new(self.then), None))
    }
}

impl<N: Build<ast::Statement>> Build<ast::Statement> for IfBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Statement) -> N::Next {
        self.next.build_with(ast::Statement::If(self.condition, Box::new(self.then), Some(Box::new(result))))
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for source elements, such as statements and functions.
pub struct SourceElementBuilder<N: Build<ast::SourceElement>>(N);

impl<N: Build<ast::SourceElement>> SourceElementBuilder<N> {
    ///Build a statement.
    pub fn stmt(self) -> StatementBuilder<Self> {
        StatementBuilder::from(self)
    }

    ///Build a function.
    pub fn function<S: Into<ast::Identifier>>(self, name: S) -> FnArgnamesBuilder<FnAdapter<N>> {
        FnArgnamesBuilder::from(FnAdapter::from(name, self.0))
    }
}

impl<N: Build<ast::SourceElement>> From<N> for SourceElementBuilder<N> {
    fn from(next: N) -> SourceElementBuilder<N> {
        SourceElementBuilder(next)
    }
}

impl<N: Build<ast::SourceElement>> Build<ast::Statement> for SourceElementBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::Statement) -> N::Next {
        self.0.build_with(ast::SourceElement::Statement(result))
    }
}
