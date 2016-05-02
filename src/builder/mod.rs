//!Abstract syntax tree builders.

use std::io::{self, Write};

use ast::{SourceElement, Statement, Identifier, FunctionBody};
use print::{Print, Formatter};
use self::statement::{StatementBuilder, SourceElementBuilder};
use self::function::{FnArgnamesBuilder, FnAdapter};

pub mod statement;
pub mod expression;
pub mod function;

///The interface between builders.
pub trait Build<T> {
    ///The next value in the chain.
    type Next;

    ///Use the result from a builder.
    fn build_with(self, result: T) -> Self::Next;
}

impl<T> Build<T> for () {
    type Next = T;
    fn build_with(self, result: T) -> T {
        result
    }
}

///Script builder.
///
///This is analogous to a script file, and it's simply a collection of source
///elements, such as statements and functions.
pub struct Script(Vec<SourceElement>);

impl Script {
    ///Create a new script builder.
    pub fn new() -> Script {
        Script(vec![])
    }

    ///Add a statement to the script.
    ///
    ///```
    ///use js_builder::Script;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Script::new()
    ///    .stmt().var().inited("a").add().id("b").id("c").build().build()
    ///    .stmt().return_().mul().id("a").id("b").build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "var a = b + c;return a * b;",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn stmt(self) -> StatementBuilder<Script> {
        StatementBuilder::from(self)
    }

    ///Same as `stmt`, but without moving the script builder.
    pub fn add_stmt(&mut self) -> StatementBuilder<&mut Script> {
        StatementBuilder::from(self)
    }

    ///Add a pre-built statement.
    pub fn with_stmt(&mut self, stmt: Statement) -> &mut Script {
        self.0.push(SourceElement::Statement(stmt));
        self
    }

    ///Add a function to the script.
    ///
    ///```
    ///use js_builder::Script;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Script::new()
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
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "function do_something(a, b, c) {a = b + c;}return do_something(a, 1, 2);",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn function<S: Into<Identifier>>(self, name: S) -> FnArgnamesBuilder<FnAdapter<Script>> {
        SourceElementBuilder::from(self).function(name)
    }

    ///Same as `function`, but without moving the script builder.
    pub fn add_function<S: Into<Identifier>>(&mut self, name: S) -> FnArgnamesBuilder<FnAdapter<&mut Script>> {
        SourceElementBuilder::from(self).function(name)
    }

    ///Add a function with pre-built arguments and body.
    pub fn with_function<S: Into<Identifier>>(&mut self, name: S, args: Vec<Identifier>, body: FunctionBody) {
        self.0.push(SourceElement::Function(name.into(), args, body))
    }

    ///Add a pre-built source element.
    pub fn with_source_element(&mut self, element: SourceElement) {
        self.0.push(element);
    }
}

impl Print for Script {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        for element in &self.0 {
            try!(element.print_with(f));
        }

        Ok(())
    }
}

impl From<Script> for FunctionBody {
    fn from(script: Script) -> FunctionBody {
        FunctionBody {
            elements: script.0,
        }
    }
}

impl Build<Statement> for Script {
    type Next = Script;

    fn build_with(mut self, result: Statement) -> Script {
        self.0.push(SourceElement::Statement(result));
        self
    }
}

impl Build<SourceElement> for Script {
    type Next = Script;

    fn build_with(mut self, result: SourceElement) -> Script {
        self.0.push(result);
        self
    }
}

impl<'a> Build<Statement> for &'a mut Script {
    type Next = &'a mut Script;

    fn build_with(self, result: Statement) -> &'a mut Script {
        self.0.push(SourceElement::Statement(result));
        self
    }
}

impl<'a> Build<SourceElement> for &'a mut Script {
    type Next = &'a mut Script;

    fn build_with(mut self, result: SourceElement) -> &'a mut Script {
        self.0.push(result);
        self
    }
}
