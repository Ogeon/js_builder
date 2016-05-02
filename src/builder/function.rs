//!Function related builders.

use ast;
use builder::Build;
use builder::statement::{StatementBuilder, SourceElementBuilder};
use builder::expression::{ExpressionBuilder, PathType};

///An adapter that holds a function signature.
pub struct FnAdapter<N: Build<ast::SourceElement>>{
    name: ast::Identifier,
    args: Vec<ast::Identifier>,
    next: N,
}

impl<N: Build<ast::SourceElement>> FnAdapter<N> {
    ///Create a new `FnAdapter` from a function name.
    pub fn from<S: Into<ast::Identifier>>(name: S, next: N) -> FnAdapter<N> {
        FnAdapter {
            name: name.into(),
            args: vec![],
            next: next,
        }
    }
}

impl<N: Build<ast::SourceElement>> Build<Vec<ast::Identifier>> for FnAdapter<N> {
    type Next = FnBodyBuilder<Self>;

    fn build_with(mut self, result: Vec<ast::Identifier>) -> FnBodyBuilder<Self> {
        self.args = result;
        FnBodyBuilder::from(self)
    }
}

impl<N: Build<ast::SourceElement>> Build<ast::FunctionBody> for FnAdapter<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::FunctionBody) -> N::Next {
        self.next.build_with(ast::SourceElement::Function(self.name, self.args, result))
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for function argument names.
pub struct FnArgnamesBuilder<N: Build<Vec<ast::Identifier>>>(Vec<ast::Identifier>, N);

impl<N: Build<Vec<ast::Identifier>>> FnArgnamesBuilder<N> {
    ///Add an argument to the argument list.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().function()
    ///    .arg("a")
    ///    .arg("b")
    ///    .arg("c")
    ///    .build() //finish the argument list and start building the function body
    ///    .stmt().expr().assign().id("a").add().id("b").id("c").build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "function(a, b, c) {a = b + c;}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn arg<S: Into<ast::Identifier>>(mut self, id: S) -> FnArgnamesBuilder<N> {
        self.0.push(id.into());
        self
    }

    ///Same as `arg`, but without moving the builder.
    pub fn add_arg<S: Into<ast::Identifier>>(&mut self, id: S) -> &mut FnArgnamesBuilder<N> {
        self.0.push(id.into());
        self
    }

    ///Finish building the function argument list.
    pub fn build(self) -> N::Next {
        self.1.build_with(self.0)
    }
}

impl<N: Build<Vec<ast::Identifier>>> From<N> for FnArgnamesBuilder<N> {
    fn from(next: N) -> FnArgnamesBuilder<N> {
        FnArgnamesBuilder(vec![], next)
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for function bodies.
pub struct FnBodyBuilder<N: Build<ast::FunctionBody>>(Vec<ast::SourceElement>, N);

impl<N: Build<ast::FunctionBody>> FnBodyBuilder<N> {
    ///Add a statement to the function body.
    ///
    ///```
    ///use js_builder::ast::FunctionBody;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///FunctionBody::build()
    ///    .stmt().var().inited("a").add().id("b").id("c").build().build()
    ///    .stmt().return_().mul().id("a").id("b").build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{var a = b + c;return a * b;}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn stmt(self) -> StatementBuilder<Self> {
        StatementBuilder::from(self)
    }

    ///Same as `stmt`, but without moving the builder.
    pub fn add_stmt(&mut self) -> StatementBuilder<&mut Self> {
        StatementBuilder::from(self)
    }

    ///Add a pre-built statement to the function body.
    pub fn with_stmt(&mut self, stmt: ast::Statement) -> &mut FnBodyBuilder<N> {
        self.0.push(ast::SourceElement::Statement(stmt));
        self
    }

    ///Add a function to the function body.
    ///
    ///```
    ///use js_builder::ast::FunctionBody;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///FunctionBody::build()
    ///    .function("do_something")
    ///        .arg("a")
    ///        .arg("b")
    ///        .arg("c")
    ///        .build()
    ///        .stmt().expr().assign().id("a").add().id("b").id("c").build()
    ///        .build()
    ///    .stmt().return_().path()
    ///        .id("do_something").call()
    ///            .arg().id("a")
    ///            .arg().number(1.0)
    ///            .arg().number(2.0)
    ///            .build()
    ///        .build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{function do_something(a, b, c) {a = b + c;}return do_something(a, 1, 2);}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn function<S: Into<ast::Identifier>>(self, name: S) -> FnArgnamesBuilder<FnAdapter<Self>> {
        SourceElementBuilder::from(self).function(name)
    }

    ///Same as `function`, but without moving the builder.
    pub fn add_function<S: Into<ast::Identifier>>(&mut self, name: S) -> FnArgnamesBuilder<FnAdapter<&mut Self>> {
        SourceElementBuilder::from(self).function(name)
    }

    ///Add an almost pre-built function to the function body.
    pub fn with_function<S: Into<ast::Identifier>>(&mut self, name: S, args: Vec<ast::Identifier>, body: ast::FunctionBody) {
        self.0.push(ast::SourceElement::Function(name.into(), args, body))
    }

    ///Finish building the function body.
    pub fn build(self) -> N::Next {
        self.1.build_with(ast::FunctionBody {
            elements: self.0,
        })
    }

    ///Use a pre-built function body, instead.
    pub fn build_body(self, body: ast::FunctionBody) -> N::Next {
        self.1.build_with(body)
    }
}

impl<N: Build<ast::FunctionBody>> From<N> for FnBodyBuilder<N> {
    fn from(next: N) -> FnBodyBuilder<N> {
        FnBodyBuilder(vec![], next)
    }
}

impl<N: Build<ast::FunctionBody>> Build<ast::Statement> for FnBodyBuilder<N> {
    type Next = FnBodyBuilder<N>;

    fn build_with(self, result: ast::Statement) -> FnBodyBuilder<N> {
        self.build_with(ast::SourceElement::Statement(result))
    }
}

impl<N: Build<ast::FunctionBody>> Build<ast::SourceElement> for FnBodyBuilder<N> {
    type Next = FnBodyBuilder<N>;

    fn build_with(mut self, result: ast::SourceElement) -> FnBodyBuilder<N> {
        self.0.push(result);
        self
    }
}

impl<'a, N: Build<ast::FunctionBody>> Build<ast::Statement> for &'a mut FnBodyBuilder<N> {
    type Next = &'a mut FnBodyBuilder<N>;

    fn build_with(self, result: ast::Statement) -> &'a mut FnBodyBuilder<N> {
        self.build_with(ast::SourceElement::Statement(result))
    }
}

impl<'a, N: Build<ast::FunctionBody>> Build<ast::SourceElement> for &'a mut FnBodyBuilder<N> {
    type Next = &'a mut FnBodyBuilder<N>;

    fn build_with(mut self, result: ast::SourceElement) -> &'a mut FnBodyBuilder<N> {
        self.0.push(result);
        self
    }
}

///The parts of a function call.
pub struct FnCall<E> {
    ///The function access path.
    pub access_expr: E,

    ///The function call arguments.
    pub args: Vec<ast::Expression>,
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for function calls.
pub struct FnCallBuilder<E: PathType, N: Build<FnCall<E>>> {
    access_expr: E,
    args: Vec<ast::Expression>,
    next: N,
}

impl<E: PathType, N: Build<FnCall<E>>> FnCallBuilder<E, N> {
    ///Add an argument to the function call.
    pub fn arg(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder::from(self)
    }

    ///Same as `arg`, but without moving the builder.
    pub fn add_arg(&mut self) -> ExpressionBuilder<&mut Self> {
        ExpressionBuilder::from(self)
    }

    ///Add a pre-built expression as an argument.
    pub fn with_arg(&mut self, arg: ast::Expression) -> &mut FnCallBuilder<E, N> {
        self.args.push(arg);
        self
    }

    ///Finish build the function call.
    pub fn build(self) -> N::Next {
        self.next.build_with(FnCall {
            access_expr: self.access_expr,
            args: self.args,
        })
    }

    ///Create a new function call builder from a path and a builder.
    pub fn from(path: E, next: N) -> FnCallBuilder<E, N> {
        FnCallBuilder {
            access_expr: path,
            args: vec![],
            next: next,
        }
    }
}

impl<E: PathType, N: Build<FnCall<E>>> Build<ast::Expression> for FnCallBuilder<E, N> {
    type Next = FnCallBuilder<E, N>;

    fn build_with(mut self, result: ast::Expression) -> FnCallBuilder<E, N> {
        self.args.push(result);
        self
    }
}

impl<'a, E: PathType, N: Build<FnCall<E>>> Build<ast::Expression> for &'a mut FnCallBuilder<E, N> {
    type Next = &'a mut FnCallBuilder<E, N>;

    fn build_with(mut self, result: ast::Expression) -> &'a mut FnCallBuilder<E, N> {
        self.args.push(result);
        self
    }
}
