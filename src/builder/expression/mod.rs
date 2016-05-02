//!Expression related builders.

use builder::Build;
use builder::function::{FnArgnamesBuilder, FnCallBuilder, FnCall};
use ast::{
    self,
    Identifier,
    StringLiteral,
    RegularExpression,
    Expression,
    Expressions,
    LhsExpression,
    ValueExpression,
    CallExpression,
    CallBase,
    Access,
    PrimaryExpression,
    NumericLiteral,
    PropertyAssignment,
    PropertyName,
};

use self::adapter::*;

pub mod adapter;

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for expressions.
pub struct ExpressionBuilder<N: Build<Expression>>(N);

impl<N: Build<Expression>> ExpressionBuilder<N> {
    ///Build a `this` expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().this().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "this",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn this(self) -> N::Next {
        self.path().this().build()
    }

    ///Build an identifier expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().id("foo").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "foo",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn id<S: Into<Identifier>>(self, id: S) -> N::Next {
        self.path().id(id).build()
    }

    ///Build a path expression, consisting of only identifiers.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().ids(vec!["a", "b", "c"]).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a.b.c",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn ids<I: IntoIterator>(self, ids: I) -> N::Next where I::Item: Into<Identifier> {
        self.path().ids(ids).build()
    }

    ///Build a `null` expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().null().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "null",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn null(self) -> N::Next {
        self.path().null().build()
    }

    ///Build a Boolean literal expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().bool(true).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "true",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn bool(self, b: bool) -> N::Next {
        self.path().bool(b).build()
    }

    ///Build a number literal expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().number(3.14).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "3.14",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn number<Num: Into<f64>>(self, number: Num) -> N::Next {
        self.path().number(number).build()
    }

    ///Build a hexadecimal integer expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().hex_int(0x1337beef_u64).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "0x1337beef",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn hex_int<I: Into<u64>>(self, int: I) -> N::Next {
        self.path().hex_int(int).build()
    }

    ///Build a string literal expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().string("Hello, JavaScript!").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "'Hello, JavaScript!'",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn string<S: Into<StringLiteral>>(self, string: S) -> N::Next {
        self.path().string(string).build()
    }

    ///Build a regular expression literal expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().regex(r"\w+").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    r"/\w+/",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn regex<S: Into<RegularExpression>>(self, regex: S) -> N::Next {
        self.path().regex(regex).build()
    }

    ///Build an array literal expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().array()
    ///    .elem().number(123)
    ///    .elem().string("abc")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "[123, 'abc']",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn array(self) -> ArrayBuilder<Self> {
        ArrayBuilder::from(self)
    }

    ///Build an object literal expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .field().id("a").number(123)
    ///    .field().string("b").string("abc")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{a: 123,'b': 'abc'}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn object(self) -> ObjectBuilder<Self> {
        ObjectBuilder::from(self)
    }

    ///Put parenthesis around an expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().paren().string("wow").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "('wow')",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn paren(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder::from(self)
    }

    ///Build a path expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().path()
    ///    .string("x")
    ///    .id("y")
    ///    .index().id("z")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "'x'.y[z]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn path(self) -> PathBaseBuilder<Self> {
        PathBaseBuilder(self)
    }

    ///Build a constructor call expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().new().id("Point").build()
    ///    .arg().id("x")
    ///    .arg().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "new Point(x, y)",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn new(self) -> PathBaseBuilder<FnAccessAdapter<Self>> {
        PathBaseBuilder(FnAccessAdapter::from(self))
    }

    ///Build a constructor call expression, without an argument list.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().new_value().path().id("Point").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "new Point",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn new_value(self) -> ValueExprBuilder<ValueExprBuilder<Self>> {
        ValueExprBuilder(self).new_value()
    }

    ///Build a function expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().function()
    ///    .arg("a")
    ///    .arg("b")
    ///    .build()
    ///    .stmt().return_().add().id("a").id("b").build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "function(a, b) {return a + b;}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn function(self) -> FnArgnamesBuilder<FnExprAdapter<Self>> {
        FnArgnamesBuilder::from(FnExprAdapter::from(self))
    }

    ///Build a named function expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().named_function("do_something")
    ///    .arg("a")
    ///    .arg("b")
    ///    .build()
    ///    .stmt().return_().add().id("a").id("b").build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "function do_something(a, b) {return a + b;}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn named_function<S: Into<Identifier>>(self, id: S) -> FnArgnamesBuilder<FnExprAdapter<Self>> {
        FnArgnamesBuilder::from(FnExprAdapter::with_name(id, self))
    }

    ///Build an assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a = b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::from(self.0))
    }

    ///Build an addition assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().add_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a += b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn add_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::AddAssign, self.0))
    }

    ///Build a subtraction assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().sub_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a -= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn sub_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::SubAssign, self.0))
    }

    ///Build a multiplication assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().mul_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a *= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn mul_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::MulAssign, self.0))
    }

    ///Build a division assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().div_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a /= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn div_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::DivAssign, self.0))
    }

    ///Build a modulo (remainder) assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().mod_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a %= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn mod_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::ModAssign, self.0))
    }

    ///Build a left shift assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().lsh_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a <<= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn lsh_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::LshAssign, self.0))
    }

    ///Build a right shift assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().rsh_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a >>= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn rsh_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::RshAssign, self.0))
    }

    ///Build an unsigned right shift assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().ursh_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a >>>= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn ursh_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::UrshAssign, self.0))
    }

    ///Build a bitwise `and` assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().and_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a &= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn and_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::AndAssign, self.0))
    }

    ///Build a bitwise `or` assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().or_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a |= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn or_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::OrAssign, self.0))
    }

    ///Build a bitwise `xor` assignment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().xor_assign().id("a").id("b").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a ^= b",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn xor_assign(self) -> LhsExprBuilder<AssignAdapter<N>> {
        LhsExprBuilder(AssignAdapter::with_op(ast::AssignmentOp::XorAssign, self.0))
    }

    ///Build an addition expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().add().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a + b + c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn add(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Add, self.0))
    }

    ///Build a subtraction expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().sub().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a - b - c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn sub(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Sub, self.0))
    }

    ///Build a multiplication expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().mul().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a * b * c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn mul(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Mul, self.0))
    }

    ///Build a division expression. At least two sub-expressions are required,
    ///and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().div().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a / b / c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn div(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Div, self.0))
    }

    ///Build a modulo (reminder) expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().mod_().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a % b % c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn mod_(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Mod, self.0))
    }

    ///Build a left shift expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().lsh().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a << b << c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn lsh(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Lsh, self.0))
    }

    ///Build a right shift expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().rsh().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a >> b >> c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn rsh(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Rsh, self.0))
    }

    ///Build an unsigned right shift expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().ursh().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a >>> b >>> c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn ursh(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Ursh, self.0))
    }

    ///Build a less than expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().lt().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a < b < c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn lt(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Lt, self.0))
    }

    ///Build a greater than expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().gt().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a > b > c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn gt(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Gt, self.0))
    }

    ///Build a less than or equal expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().lt_eq().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a <= b <= c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn lt_eq(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Leq, self.0))
    }

    ///Build a greater than or equal expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().gt_eq().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a >= b >= c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn gt_eq(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Geq, self.0))
    }

    ///Build an equality expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().eq().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a == b == c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn eq(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Eq, self.0))
    }

    ///Build an inequality expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().not_eq().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a != b != c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn not_eq(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::NotEq, self.0))
    }

    ///Build an identity expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().identical().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a === b === c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn identical(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Identical, self.0))
    }

    ///Build a nonidentity expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().not_identical().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a !== b !== c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn not_identical(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::NotIdentical, self.0))
    }

    ///Build a logical `and` expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().and().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a && b && c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn and(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::LogicalAnd, self.0))
    }

    ///Build a logical `or` expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().or().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a || b || c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn or(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::LogicalOr, self.0))
    }

    ///Build a bitwise `and` expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().bit_and().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a & b & c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn bit_and(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::BitwiseAnd, self.0))
    }

    ///Build a bitwise `or` expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().bit_or().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a | b | c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn bit_or(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::BitwiseOr, self.0))
    }

    ///Build a bitwise `xor` expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().bit_xor().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a ^ b ^ c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn bit_xor(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::BitwiseXor, self.0))
    }

    ///Build an `instanceof` expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().instanceof().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a instanceof b instanceof c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn instanceof(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::Instanceof, self.0))
    }

    ///Build an `in` expression. At least two sub-expressions are
    ///required, and more can be added. See
    ///[`BinaryExprBuilder`](struct.BinaryExprBuilder.html) for more info.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().in_().id("a").id("b").expr().id("c").build().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a in b in c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn in_(self) -> ExpressionBuilder<BinaryLhsAdapter<N>> {
        ExpressionBuilder(BinaryLhsAdapter::from(ast::BinaryOp::In, self.0))
    }

    ///Build a prefix increment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().pre_incr().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "++a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn pre_incr(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::PreIncr, self.0))
    }

    ///Build a postfix increment expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().post_incr().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a++",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn post_incr(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::PostIncr, self.0))
    }

    ///Build a prefix decrement expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().pre_decr().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "--a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn pre_decr(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::PreDecr, self.0))
    }

    ///Build a postfix decrement expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().post_decr().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a--",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn post_decr(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::PostDecr, self.0))
    }

    ///Build a `delete` expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().delete().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "delete a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn delete(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::Delete, self.0))
    }

    ///Build a `void` expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().void().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "void a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn void(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::Void, self.0))
    }

    ///Build a `typeof` expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().typeof_().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "typeof a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn typeof_(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::Typeof, self.0))
    }

    ///Build a unary plus expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().plus().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "+a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn plus(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::Plus, self.0))
    }

    ///Build a negation expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().neg().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "-a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn neg(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::Minus, self.0))
    }

    ///Build a logical `not` expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().not().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "!a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn not(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::LogicalNot, self.0))
    }

    ///Build a bitwise `not` expression.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().bit_not().id("a").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "~a",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn bit_not(self) -> LhsExprBuilder<UnaryAdapter<N>> {
        LhsExprBuilder(UnaryAdapter::from(ast::UnaryOp::BitwiseNot, self.0))
    }

    ///Use a pre-built expression, instead.
    pub fn build_expr(self, expression: Expression) -> N::Next {
        self.0.build_with(expression)
    }
}

impl<N: Build<Expression> + Build<Expressions>> ExpressionBuilder<N> {
    ///Build a comma separated expression chain.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().comma()
    ///    .expr().id("a")
    ///    .expr().id("b")
    ///    .expr().id("c")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a, b, c",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn comma(self) -> ExpressionsBuilder<N> {
        ExpressionsBuilder(Expressions { exprs: vec![] }, self.0)
    }
}

impl<N: Build<Expression>> From<N> for ExpressionBuilder<N> {
    fn from(next: N) -> ExpressionBuilder<N> {
        ExpressionBuilder(next)
    }
}

impl<N: Build<Expression>> Build<PrimaryExpression> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: PrimaryExpression) -> N::Next {
        self.0.build_with(Expression::Lhs(LhsExpression::Value(ValueExpression::Path(ast::PathExpression {
            base: ast::PathBase::Primary(result),
            access: vec![]
        }))))
    }
}

impl<N: Build<Expression>> Build<ValueExpression> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: ValueExpression) -> N::Next {
        self.0.build_with(Expression::Lhs(LhsExpression::Value(result)))
    }
}

impl<N: Build<Expression>> Build<ast::PathExpression> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::PathExpression) -> N::Next {
        self.build_with(ValueExpression::Path(result))
    }
}

impl<N: Build<Expression>> Build<CallExpression> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: CallExpression) -> N::Next {
        self.0.build_with(Expression::Lhs(LhsExpression::Call(result)))
    }
}

impl<N: Build<Expression>> Build<FnExprDecl> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: FnExprDecl) -> N::Next {
        self.build_with(ast::PathExpression {
            base: ast::PathBase::Function(result.name, result.args, result.body),
            access: vec![],
        })
    }
}

impl<N: Build<Expression>> Build<Vec<Expression>> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Vec<Expression>) -> N::Next {
        self.build_with(PrimaryExpression::Array(result))
    }
}

impl<N: Build<Expression>> Build<Vec<PropertyAssignment>> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Vec<PropertyAssignment>) -> N::Next {
        self.build_with(PrimaryExpression::Object(result))
    }
}

impl<N: Build<Expression>> Build<Expression> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Expression) -> N::Next {
        self.build_with(Expressions {
            exprs: vec![result],
        })
    }
}

impl<N: Build<Expression>> Build<Expressions> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Expressions) -> N::Next {
        self.build_with(PrimaryExpression::Parenthesis(result))
    }
}

impl<N: Build<Expression>> Build<FnCall<ast::PathExpression>> for ExpressionBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: FnCall<ast::PathExpression>) -> N::Next {
        self.build_with(ast::PathExpression {
            base: ast::PathBase::New(Box::new(result.access_expr), result.args),
            access: vec![]
        })
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for path expressions.
pub struct PathBuilder<E: PathType, N: Build<E>>(E, N);

impl<E: PathType, N: Build<E>> PathBuilder<E, N> {
    ///Access a member, using the dot notation.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().id("a")
    ///    .id("b")
    ///    .id("c")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a.b.c",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn id<S: Into<Identifier>>(mut self, id: S) -> PathBuilder<E, N> {
        self.0.id(id.into());
        self
    }

    ///Same as `id`, but without moving the builder.
    pub fn add_id<S: Into<Identifier>>(&mut self, id: S) -> &mut PathBuilder<E, N> {
        self.0.id(id.into());
        self
    }

    ///Access a member, using the bracket notation.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().id("a")
    ///    .index().id("b")
    ///    .index().id("c")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a[b][c]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn index(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder(self)
    }

    ///Same as `index`, but without moving the builder.
    pub fn add_index(&mut self) -> ExpressionBuilder<&mut Self> {
        ExpressionBuilder(self)
    }

    ///Index the path, using a pre-built expression chain.
    pub fn with_index(&mut self, expr: Expressions) -> &mut PathBuilder<E, N> {
        self.0.index(expr);
        self
    }

    ///Call the current path as a function, and continue building it.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().id("a")
    ///    .id("b")
    ///    .id("c")
    ///    .call()
    ///    .arg().id("x")
    ///    .arg().id("y")
    ///    .build()
    ///    .id("toString")
    ///    .call().build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a.b.c(x, y).toString()",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn call(self) -> FnCallBuilder<E, CallAdapter<N>> where
        N: Build<CallExpression>,
    {
        FnCallBuilder::from(self.0, CallAdapter::from(self.1))
    }

    ///Finish building the path.
    pub fn build(self) -> N::Next {
        self.1.build_with(self.0)
    }
}

impl<E: PathType, N: Build<E>> Build<Expression> for PathBuilder<E, N> {
    type Next = PathBuilder<E, N>;

    fn build_with(self, result: Expression) -> PathBuilder<E, N> {
        self.build_with(Expressions::from(vec![result]))
    }
}

impl<E: PathType, N: Build<E>> Build<Expressions> for PathBuilder<E, N> {
    type Next = PathBuilder<E, N>;

    fn build_with(mut self, result: Expressions) -> PathBuilder<E, N> {
        self.0.index(result);
        self
    }
}

impl<'a, E: PathType, N: Build<E>> Build<Expression> for &'a mut PathBuilder<E, N> {
    type Next = &'a mut PathBuilder<E, N>;

    fn build_with(self, result: Expression) -> &'a mut PathBuilder<E, N> {
        self.build_with(Expressions::from(vec![result]))
    }
}

impl<'a, E: PathType, N: Build<E>> Build<Expressions> for &'a mut PathBuilder<E, N> {
    type Next = &'a mut PathBuilder<E, N>;

    fn build_with(mut self, result: Expressions) -> &'a mut PathBuilder<E, N> {
        self.0.index(result);
        self
    }
}

///Common properties of path like expressions.
pub trait PathType {
    ///Add an identifier to the path.
    fn id(&mut self, id: Identifier);
    ///Add an index to the path.
    fn index(&mut self, exprs: Expressions);
    ///Turn the path into the base for a function call.
    fn into_call_base(self) -> CallBase;
}

impl PathType for ast::PathExpression {
    fn id(&mut self, id: Identifier) {
        self.access.push(Access::Dot(id));
    }

    fn index(&mut self, exprs: Expressions) {
        self.access.push(Access::Bracket(exprs));
    }

    fn into_call_base(self) -> CallBase {
        CallBase::Path(self)
    }
}

impl PathType for CallExpression {
    fn id(&mut self, id: Identifier) {
        self.access.push(Access::Dot(id));
    }

    fn index(&mut self, exprs: Expressions) {
        self.access.push(Access::Bracket(exprs));
    }

    fn into_call_base(self) -> CallBase {
        CallBase::Call(Box::new(self))
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for the first part of a path.
pub struct PathBaseBuilder<N: Build<ast::PathExpression>>(N);

impl<N: Build<ast::PathExpression>> PathBaseBuilder<N> {
    ///Build a path that begins with `this`.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().this()
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "this.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn this(self) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::This),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with an identifier.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().id("foo")
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "foo.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn id<S: Into<Identifier>>(self, id: S) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::Identifier(id.into())),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with `null`.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().null()
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "null.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn null(self) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::Null),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with a Boolean value.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().bool(false)
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "false.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn bool(self, b: bool) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::Bool(b)),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with a number. You may want to wrap the number
    ///in parenthesis first, to avoid ambiguities. See
    ///[`paren`](#method.paren), below.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().number(3.14)
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "3.14.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn number<Num: Into<f64>>(self, number: Num) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::Numeric(ast::NumericLiteral::Decimal(number.into()))),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with a hexadecimal integer.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().hex_int(0x1337beef_u64)
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "0x1337beef.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn hex_int<I: Into<u64>>(self, int: I) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::Numeric(ast::NumericLiteral::HexIntger(int.into()))),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with a string.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().string("hello")
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "'hello'.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn string<S: Into<StringLiteral>>(self, string: S) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::String(string.into())),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with a regular expression.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().regex(r"\w+")
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    r"/\w+/.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn regex<S: Into<RegularExpression>>(self, regex: S) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(ast::PrimaryExpression::RegularExpression(regex.into())),
            access: vec![],
        }, self.0)
    }

    ///Build a path that begins with an array.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().array()
    ///    .elem().id("a")
    ///    .elem().id("b")
    ///    .elem().id("c")
    ///    .build()
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    r"[a, b, c].x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn array(self) -> ArrayBuilder<Self> {
        ArrayBuilder::from(self)
    }

    ///Build a path that begins with an object.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().object()
    ///    .field().id("a").number(1)
    ///    .field().id("b").number(2)
    ///    .field().id("c").number(3)
    ///    .build()
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    r"{a: 1,b: 2,c: 3}.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn object(self) -> ObjectBuilder<Self> {
        ObjectBuilder::from(self)
    }

    ///Build a path that begins with an expression in parenthesis.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().paren().number(4)
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "(4).x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn paren(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder::from(self)
    }

    ///Build a path that begins with a function expression.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().function()
    ///    .arg("a")
    ///    .arg("b")
    ///    .build()
    ///    .stmt().return_().add().id("a").id("b").build()
    ///    .build()
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "(function(a, b) {return a + b;}).x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn function(self) -> FnArgnamesBuilder<FnExprAdapter<Self>> {
        FnArgnamesBuilder::from(FnExprAdapter::from(self))
    }

    ///Build a path that begins with a function expression.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().named_function("do_something")
    ///    .arg("a")
    ///    .arg("b")
    ///    .build()
    ///    .stmt().return_().add().id("a").id("b").build()
    ///    .build()
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "(function do_something(a, b) {return a + b;}).x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn named_function<S: Into<Identifier>>(self, id: S) -> FnArgnamesBuilder<FnExprAdapter<Self>> {
        FnArgnamesBuilder::from(FnExprAdapter::with_name(id, self))
    }

    ///Build a path that begins with a constructor call.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().new().id("Foo").build()
    ///    .arg().id("a")
    ///    .arg().id("b")
    ///    .build()
    ///    .id("x")
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "new Foo(a, b).x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn new(self) -> PathBaseBuilder<FnAccessAdapter<Self>> {
        PathBaseBuilder(FnAccessAdapter::from(self))
    }

    ///Build a path that begins with a sequence of identifiers.
    ///
    ///```
    ///use js_builder::ast::PathExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///PathExpression::build().ids(vec!["foo", "x"])
    ///    .index().id("y")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "foo.x[y]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn ids<I: IntoIterator>(self, ids: I) -> PathBuilder<ast::PathExpression, N> where
        I::Item: Into<Identifier>,
    {
        let mut ids = ids.into_iter();
        let first = ids.next().expect("ExpressionBuilder::ids expected a non-empty path");
        let mut builder = self.id(first);
        for id in ids {
            builder.add_id(id);
        }
        builder
    }
}

impl<N: Build<ast::PathExpression>> From<N> for PathBaseBuilder<N> {
    fn from(next: N) -> PathBaseBuilder<N> {
        PathBaseBuilder(next)
    }
}

impl<N: Build<ast::PathExpression>> Build<Vec<Expression>> for PathBaseBuilder<N> {
    type Next = PathBuilder<ast::PathExpression, N>;

    fn build_with(self, result: Vec<Expression>) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(PrimaryExpression::Array(result)),
            access: vec![],
        }, self.0)
    }
}

impl<N: Build<ast::PathExpression>> Build<Vec<PropertyAssignment>> for PathBaseBuilder<N> {
    type Next = PathBuilder<ast::PathExpression, N>;

    fn build_with(self, result: Vec<PropertyAssignment>) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(PrimaryExpression::Object(result)),
            access: vec![],
        }, self.0)
    }
}

impl<N: Build<ast::PathExpression>> Build<Expression> for PathBaseBuilder<N> {
    type Next = PathBuilder<ast::PathExpression, N>;

    fn build_with(self, result: Expression) -> PathBuilder<ast::PathExpression, N> {
        self.build_with(Expressions {
            exprs: vec![result],
        })
    }
}

impl<N: Build<ast::PathExpression>> Build<Expressions> for PathBaseBuilder<N> {
    type Next = PathBuilder<ast::PathExpression, N>;

    fn build_with(self, result: Expressions) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(PrimaryExpression::Parenthesis(result)),
            access: vec![],
        }, self.0)
    }
}

impl<N: Build<ast::PathExpression>> Build<FnExprDecl> for PathBaseBuilder<N> {
    type Next = PathBuilder<ast::PathExpression, N>;

    fn build_with(self, result: FnExprDecl) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Function(result.name, result.args, result.body),
            access: vec![],
        }, self.0)
    }
}

impl<N: Build<ast::PathExpression>> Build<FnCall<ast::PathExpression>> for PathBaseBuilder<N> {
    type Next = PathBuilder<ast::PathExpression, N>;

    fn build_with(self, result: FnCall<ast::PathExpression>) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::New(Box::new(result.access_expr), result.args),
            access: vec![],
        }, self.0)
    }
}

impl<N: Build<ast::PathExpression>> Build<PrimaryExpression> for PathBaseBuilder<N> {
    type Next = PathBuilder<ast::PathExpression, N>;

    fn build_with(self, result: PrimaryExpression) -> PathBuilder<ast::PathExpression, N> {
        PathBuilder(ast::PathExpression {
            base: ast::PathBase::Primary(result),
            access: vec![],
        }, self.0)
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for comma separated expression chains.
pub struct ExpressionsBuilder<N: Build<Expressions>>(Expressions, N);

impl<N: Build<Expressions>> ExpressionsBuilder<N> {
    ///Add an expression to the chain.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().comma()
    ///    .expr().assign().ids(vec!["foo", "x"]).id("bar")
    ///    .expr().add().ids(vec!["foo", "x"]).number(2).build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "foo.x = bar, foo.x + 2",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn expr(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder::from(self)
    }

    ///Same as `expr`, but without moving the builder.
    pub fn add_expr(&mut self) -> ExpressionBuilder<&mut Self> {
        ExpressionBuilder::from(self)
    }

    ///Add a pre-built expression to the chain.
    pub fn with_expr(&mut self, expr: Expression) -> &mut ExpressionsBuilder<N> {
        self.0.exprs.push(expr);
        self
    }

    ///Finnish building the expression chain.
    pub fn build(self) -> N::Next {
        self.1.build_with(self.0)
    }
}

impl<N: Build<Expressions>> Build<Expression> for ExpressionsBuilder<N> {
    type Next = ExpressionsBuilder<N>;

    fn build_with(mut self, result: Expression) -> ExpressionsBuilder<N> {
        self.0.exprs.push(result);
        self
    }
}

impl<'a, N: Build<Expressions>> Build<Expression> for &'a mut ExpressionsBuilder<N> {
    type Next = &'a mut ExpressionsBuilder<N>;

    fn build_with(mut self, result: Expression) -> &'a mut ExpressionsBuilder<N> {
        self.0.exprs.push(result);
        self
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for array literals.
pub struct ArrayBuilder<N: Build<Vec<Expression>>>(Vec<Expression>, N);

impl<N: Build<Vec<Expression>>> ArrayBuilder<N> {
    ///Add an element to the array.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().array()
    ///    .elem().number(1)
    ///    .elem().number(2)
    ///    .elem().number(3)
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "[1, 2, 3]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn elem(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder::from(self)
    }

    ///Same as `elem`, but without moving the builder.
    pub fn add_elem(&mut self) -> ExpressionBuilder<&mut Self> {
        ExpressionBuilder::from(self)
    }

    ///Add a pre-built element to the array.
    pub fn with_elem(&mut self, expr: Expression) -> &mut ArrayBuilder<N> {
        self.0.push(expr);
        self
    }

    ///Finish building the array.
    pub fn build(self) -> N::Next {
        self.1.build_with(self.0)
    }
}

impl<N: Build<Vec<Expression>>> From<N> for ArrayBuilder<N> {
    fn from(next: N) -> ArrayBuilder<N> {
        ArrayBuilder(vec![], next)
    }
}

impl<N: Build<Vec<Expression>>> Build<Expression> for ArrayBuilder<N> {
    type Next = ArrayBuilder<N>;

    fn build_with(mut self, result: Expression) -> ArrayBuilder<N> {
        self.0.push(result);
        self
    }
}

impl<'a, N: Build<Vec<Expression>>> Build<Expression> for &'a mut ArrayBuilder<N> {
    type Next = &'a mut ArrayBuilder<N>;

    fn build_with(mut self, result: Expression) -> &'a mut ArrayBuilder<N> {
        self.0.push(result);
        self
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for object literals.
pub struct ObjectBuilder<N: Build<Vec<PropertyAssignment>>>(Vec<PropertyAssignment>, N);

impl<N: Build<Vec<PropertyAssignment>>> ObjectBuilder<N> {
    ///Add an field to the object.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .field().id("foo").number(1)
    ///    .field().string("bar").number(2)
    ///    .field().number(3).number(3)
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{foo: 1,'bar': 2,3: 3}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn field(self) -> PropNameBuilder<PropAssignAdapter<Self>> {
        PropNameBuilder(PropAssignAdapter::from(self))
    }

    ///Same as `field`, but without moving the builder.
    pub fn add_field(&mut self) -> PropNameBuilder<PropAssignAdapter<&mut Self>> {
        PropNameBuilder(PropAssignAdapter::from(self))
    }

    ///Add a getter to the object.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .getter().id("foo").stmt().return_().number(42).build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{get foo() {return 42;}}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn getter(self) -> PropNameBuilder<GetterAdapter<Self>> {
        PropNameBuilder(GetterAdapter::from(self))
    }

    ///Same as `getter`, but without moving the builder.
    pub fn add_getter(&mut self) -> PropNameBuilder<GetterAdapter<&mut Self>> {
        PropNameBuilder(GetterAdapter::from(self))
    }

    ///Add a getter to the object.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .setter("x").id("foo").stmt().expr().assign().id("bar").id("x").build()
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{set foo(x) {bar = x;}}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn setter<S: Into<Identifier>>(self, arg: S) -> PropNameBuilder<SetterAdapter<Self>> {
        PropNameBuilder(SetterAdapter::from(arg.into(), self))
    }

    ///Same as `setter`, but without moving the builder.
    pub fn add_setter<S: Into<Identifier>>(&mut self, arg: S) -> PropNameBuilder<SetterAdapter<&mut Self>> {
        PropNameBuilder(SetterAdapter::from(arg.into(), self))
    }

    ///Finish building the object.
    pub fn build(self) -> N::Next {
        self.1.build_with(self.0)
    }
}

impl<N: Build<Vec<PropertyAssignment>>> From<N> for ObjectBuilder<N> {
    fn from(next: N) -> ObjectBuilder<N> {
        ObjectBuilder(vec![], next)
    }
}

impl<N: Build<Vec<PropertyAssignment>>> Build<PropertyAssignment> for ObjectBuilder<N> {
    type Next = ObjectBuilder<N>;

    fn build_with(mut self, result: PropertyAssignment) -> ObjectBuilder<N> {
        self.0.push(result);
        self
    }
}

impl<'a, N: Build<Vec<PropertyAssignment>>> Build<PropertyAssignment> for &'a mut ObjectBuilder<N> {
    type Next = &'a mut ObjectBuilder<N>;

    fn build_with(self, result: PropertyAssignment) -> &'a mut ObjectBuilder<N> {
        self.0.push(result);
        self
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for object field (property) names.
pub struct PropNameBuilder<N: Build<PropertyName>>(N);

impl<N: Build<PropertyName>> PropNameBuilder<N> {
    ///Use an identifier.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .field().id("foo").number(42)
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{foo: 42}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn id<S: Into<Identifier>>(self, id: S) -> N::Next {
        self.0.build_with(PropertyName::Identifier(id.into()))
    }

    ///Use a string.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .field().string("foo").number(42)
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{'foo': 42}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn string<S: Into<StringLiteral>>(self, string: S) -> N::Next {
        self.0.build_with(PropertyName::StringLiteral(string.into()))
    }

    ///Use a number.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .field().number(42).id("foo")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{42: foo}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn number<Num: Into<f64>>(self, number: Num) -> N::Next {
        self.0.build_with(PropertyName::NumericLiteral(NumericLiteral::Decimal(number.into())))
    }

    ///Use a hexadecimal integer.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().object()
    ///    .field().hex_int(0x1337beef_u64).id("foo")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{0x1337beef: foo}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn hex_int<I: Into<u64>>(self, int: I) -> N::Next {
        self.0.build_with(PropertyName::NumericLiteral(NumericLiteral::HexIntger(int.into())))
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for value expressions.
pub struct ValueExprBuilder<N: Build<ValueExpression>>(N);

impl<N: Build<ValueExpression>> ValueExprBuilder<N> {
    ///Build a path expression.
    ///
    ///```
    ///use js_builder::ast::ValueExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///ValueExpression::build().path()
    ///    .id("a")
    ///    .id("b")
    ///    .id("c")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a.b.c",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn path(self) -> PathBaseBuilder<Self> {
        PathBaseBuilder(self)
    }

    ///Build a constructor call expression, without an argument list.
    ///
    ///```
    ///use js_builder::ast::ValueExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///ValueExpression::build().new_value().path()
    ///    .id("a")
    ///    .id("b")
    ///    .id("c")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "new a.b.c",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn new_value(self) -> ValueExprBuilder<Self> {
        ValueExprBuilder(self)
    }
}

impl<N: Build<ValueExpression>> From<N> for ValueExprBuilder<N> {
    fn from(next: N) -> ValueExprBuilder<N> {
        ValueExprBuilder(next)
    }
}

impl<N: Build<ValueExpression>> Build<ast::PathExpression> for ValueExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::PathExpression) -> N::Next {
        self.0.build_with(ValueExpression::Path(result))
    }
}

impl<N: Build<ValueExpression>> Build<ValueExpression> for ValueExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: ValueExpression) -> N::Next {
        self.0.build_with(ValueExpression::New(Box::new(result)))
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for left hand side expressions.
pub struct LhsExprBuilder<N: Build<LhsExpression>>(N);

impl<N: Build<LhsExpression>> LhsExprBuilder<N> {
    ///Build a `this` expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().this().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "this",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn this(self) -> N::Next {
        self.path().this().build()
    }

    ///Build an identifier expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().id("foo").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "foo",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn id<S: Into<Identifier>>(self, id: S) -> N::Next {
        self.path().id(id).build()
    }

    ///Build a path expression, consisting of only identifiers.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().ids(vec!["a", "b", "c"]).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a.b.c",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn ids<I: IntoIterator>(self, ids: I) -> N::Next where I::Item: Into<Identifier> {
        self.path().ids(ids).build()
    }

    ///Build a `null` expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().null().compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "null",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn null(self) -> N::Next {
        self.path().null().build()
    }

    ///Build a Boolean literal expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().bool(true).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "true",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn bool(self, b: bool) -> N::Next {
        self.path().bool(b).build()
    }

    ///Build a number literal expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().number(3.14).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "3.14",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn number<Num: Into<f64>>(self, number: Num) -> N::Next {
        self.path().number(number).build()
    }

    ///Build a hexadecimal integer expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().hex_int(0x1337beef_u64).compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "0x1337beef",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn hex_int<I: Into<u64>>(self, int: I) -> N::Next {
        self.path().hex_int(int).build()
    }

    ///Build a string literal expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().string("Hello, JavaScript!").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "'Hello, JavaScript!'",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn string<S: Into<StringLiteral>>(self, string: S) -> N::Next {
        self.path().string(string).build()
    }

    ///Build a regular expression literal expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().regex(r"\w+").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    r"/\w+/",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn regex<S: Into<RegularExpression>>(self, regex: S) -> N::Next {
        self.path().regex(regex).build()
    }

    ///Build an array literal expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().array()
    ///    .elem().number(123)
    ///    .elem().string("abc")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "[123, 'abc']",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn array(self) -> ArrayBuilder<Self> {
        ArrayBuilder::from(self)
    }

    ///Build an object literal expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().object()
    ///    .field().id("a").number(123)
    ///    .field().string("b").string("abc")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "{a: 123,'b': 'abc'}",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn object(self) -> ObjectBuilder<Self> {
        ObjectBuilder::from(self)
    }

    ///Put parenthesis around an expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().paren().string("wow").compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "('wow')",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn paren(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder::from(self)
    }

    ///Build a path expression.
    ///
    ///```
    ///use js_builder::ast::LhsExpression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///LhsExpression::build().path()
    ///    .string("x")
    ///    .id("y")
    ///    .index().id("z")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "'x'.y[z]",
    ///    String::from_utf8_lossy(&result)
    ///);
    ///```
    pub fn path(self) -> PathBaseBuilder<Self> {
        PathBaseBuilder(self)
    }
}

impl<N: Build<LhsExpression>> From<N> for LhsExprBuilder<N> {
    fn from(next: N) -> LhsExprBuilder<N> {
        LhsExprBuilder(next)
    }
}

impl<N: Build<LhsExpression>> Build<ast::PathExpression> for LhsExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: ast::PathExpression) -> N::Next {
        self.0.build_with(LhsExpression::Value(ValueExpression::Path(result)))
    }
}

impl<N: Build<LhsExpression>> Build<CallExpression> for LhsExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: CallExpression) -> N::Next {
        self.0.build_with(LhsExpression::Call(result))
    }
}

impl<N: Build<LhsExpression>> Build<PrimaryExpression> for LhsExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: PrimaryExpression) -> N::Next {
        self.build_with(ast::PathExpression {
            base: ast::PathBase::Primary(result),
            access: vec![],
        })
    }
}

impl<N: Build<LhsExpression>> Build<Vec<Expression>> for LhsExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Vec<Expression>) -> N::Next {
        self.build_with(PrimaryExpression::Array(result))
    }
}

impl<N: Build<LhsExpression>> Build<Vec<PropertyAssignment>> for LhsExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Vec<PropertyAssignment>) -> N::Next {
        self.build_with(PrimaryExpression::Object(result))
    }
}

impl<N: Build<LhsExpression>> Build<Expression> for LhsExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Expression) -> N::Next {
        self.build_with(Expressions {
            exprs: vec![result],
        })
    }
}

impl<N: Build<LhsExpression>> Build<Expressions> for LhsExprBuilder<N> {
    type Next = N::Next;

    fn build_with(self, result: Expressions) -> N::Next {
        self.build_with(PrimaryExpression::Parenthesis(result))
    }
}

#[must_use = "builders are sometimes lazy and must be used to produce a result"]
///A builder for binary operator chains.
pub struct BinaryExprBuilder<N: Build<Expression>>(ast::BinaryOp, Vec<Expression>, N);

impl<N: Build<Expression>> BinaryExprBuilder<N> {
    ///Add an expression to the chain.
    ///
    ///```
    ///use js_builder::ast::Expression;
    ///use js_builder::Print;
    ///
    ///let mut result = Vec::new();
    ///Expression::build().add().id("a").id("b")
    ///    .expr().id("c")
    ///    .expr().id("d")
    ///    .build()
    ///    .compact_print(&mut result);
    ///
    ///assert_eq!(
    ///    "a + b + c + d",
    ///    String::from_utf8_lossy(&result)
    ///);
    pub fn expr(self) -> ExpressionBuilder<Self> {
        ExpressionBuilder(self)
    }

    ///Same as `expr`, but without moving the builder.
    pub fn add_expr(&mut self) -> ExpressionBuilder<&mut Self> {
        ExpressionBuilder(self)
    }

    ///Add a pre-built expression to the chain.
    pub fn with_expr(&mut self, expr: Expression) -> &mut Self {
        self.1.push(expr);
        self
    }

    ///Finish building the binary operator chain.
    pub fn build(self) -> N::Next {
        self.2.build_with(Expression::Binary(self.0, self.1))
    }
}

impl<N: Build<Expression>> Build<Expression> for BinaryExprBuilder<N> {
    type Next = BinaryExprBuilder<N>;

    fn build_with(mut self, result: Expression) -> BinaryExprBuilder<N> {
        self.1.push(result);
        self
    }
}

impl<'a, N: Build<Expression>> Build<Expression> for &'a mut BinaryExprBuilder<N> {
    type Next = &'a mut BinaryExprBuilder<N>;

    fn build_with(self, result: Expression) -> &'a mut BinaryExprBuilder<N> {
        self.1.push(result);
        self
    }
}
