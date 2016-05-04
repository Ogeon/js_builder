//!Abstract syntax tree components.

use std::io::{self, Write};
use std::fmt::{self, Display};
use std::fmt::Write as FmtWrite;

use string_cache::Atom;

use builder::expression::{ExpressionBuilder, PathBaseBuilder, ValueExprBuilder, LhsExprBuilder};
use builder::statement::{BlockBuilder, StatementBuilder, SourceElementBuilder};
use builder::function::FnBodyBuilder;
use print::{Print, Formatter};

///An identifier.
pub type Identifier = Atom;

///A chain of comma separated expressions.
pub struct Expressions {
    ///The comma separated expressions.
    pub exprs: Vec<Expression>,
}

impl Print for Expressions {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        let mut first = true;
        for expr in &self.exprs {
            if !first {
                try!(f.write(", "));
            }
            try!(expr.print_with(f));
            first = false;
        }
        Ok(())
    }
}

impl From<Vec<Expression>> for Expressions {
    fn from(exprs: Vec<Expression>) -> Expressions {
        Expressions {
            exprs: exprs,
        }
    }
}

///A string literal.
///
///The string is automatically escaped.
pub struct StringLiteral {
    ///Use double quotes (`"`).
    pub double_quote: bool,

    ///The string.
    pub string: Atom,
}

impl<S: Into<Atom>> From<S> for StringLiteral {
    fn from(string: S) -> StringLiteral {
        StringLiteral {
            double_quote: false,
            string: string.into(),
        }
    }
}

impl Print for StringLiteral {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        if self.double_quote {
            write!(f, r#""{}""#, EscapedStr(&self.string))
        } else {
            write!(f, r"'{}'", EscapedStr(&self.string))
        }
    }
}

struct EscapedStr<S>(S);

impl<S> Display for EscapedStr<S> where S: AsRef<str> {
    fn fmt(&self, f: &mut fmt::Formatter) ->  fmt::Result {
        for c in self.0.as_ref().chars() {
            match c {
                '\u{0008}' => try!(f.write_str("\\b")),
                '\u{0009}' => try!(f.write_str("\\t")),
                '\u{000A}' => try!(f.write_str("\\n")),
                '\u{000B}' => try!(f.write_str("\\v")),
                '\u{000C}' => try!(f.write_str("\\f")),
                '\u{000D}' => try!(f.write_str("\\r")),
                '\u{0022}' => try!(f.write_str("\\\"")),
                '\u{0027}' => try!(f.write_str("\\'")),
                '\u{005C}' => try!(f.write_str("\\\\")),
                c => try!(f.write_char(c))
            }
        }

        Ok(())
    }
}

///A number.
pub enum NumericLiteral {
    ///A regular decimal number literal.
    Decimal(f64),

    ///A hexadecimal number literal.
    HexIntger(u64),
}

impl Print for NumericLiteral {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            NumericLiteral::Decimal(d) => f.write(d),
            NumericLiteral::HexIntger(i) => write!(f, "0x{:x}", i),
        }
    }
}

///A regular expression literal.
pub struct RegularExpression {
    ///The expression body (the part between `/.../`).
    pub body: Atom,

    ///The `g` flag.
    pub global: bool,

    ///The `i` flag.
    pub ignore_case: bool,

    ///The `m` flag.
    pub multiline: bool,

    ///The `u` flag.
    pub unicode: bool,

    ///The `y` flag.
    pub sticky: bool,
}

impl<S: Into<Atom>> From<S> for RegularExpression {
    fn from(body: S) -> RegularExpression {
        RegularExpression {
            body: body.into(),
            global: false,
            ignore_case: false,
            multiline: false,
            unicode: false,
            sticky: false,
        }
    }
}

impl Print for RegularExpression {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        try!(write!(f, r"/{}/", self.body));
        if self.global {
            try!(f.write('g'));
        }
        if self.ignore_case {
            try!(f.write('i'));
        }
        if self.multiline {
            try!(f.write('m'));
        }
        if self.unicode {
            try!(f.write('u'));
        }
        if self.sticky {
            try!(f.write('y'));
        }
        Ok(())
    }
}

///A basic expression, such as a literal.
pub enum PrimaryExpression {
    ///The `this` keyword.
    This,

    ///An identifier.
    Identifier(Identifier),

    ///A `null` value.
    Null,

    ///A Boolean literal.
    Bool(bool),

    ///A number literal.
    Numeric(NumericLiteral),

    ///A string literal.
    String(StringLiteral),

    ///A regular expression literal.
    RegularExpression(RegularExpression),

    ///An array literal.
    Array(Vec<Expression>),

    ///An object literal.
    Object(Vec<PropertyAssignment>),

    ///An expression in parenthesis.
    Parenthesis(Expressions),
}

impl Print for PrimaryExpression {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            PrimaryExpression::This => f.write("this"),
            PrimaryExpression::Identifier(ref ident) => f.write(ident),
            PrimaryExpression::Null => f.write("null"),
            PrimaryExpression::Bool(ref b) => f.write(b),
            PrimaryExpression::Numeric(ref n) => n.print_with(f),
            PrimaryExpression::String(ref s) => s.print_with(f),
            PrimaryExpression::RegularExpression(ref regex) => regex.print_with(f),
            PrimaryExpression::Array(ref elements) => {
                try!(f.write('['));
                let mut first = true;
                for element in elements {
                    if !first {
                        try!(f.write(", "));
                    }
                    try!(element.print_with(f));
                    first = false;
                }
                f.write(']')
            },
            PrimaryExpression::Object(ref assignments) => {
                try!(f.write('{'));
                {
                    let mut f = f.indented();
                    let mut first = true;
                    for assignment in assignments {
                        if !first {
                            try!(f.write(','));
                        }
                        try!(f.new_line());
                        try!(assignment.print_with(&mut f));
                        first = false;
                    }
                }
                if !assignments.is_empty() {
                    try!(f.new_line());
                }
                f.write('}')
            },
            PrimaryExpression::Parenthesis(ref expr) => {
                try!(f.write('('));
                try!(expr.print_with(f));
                f.write(')')
            },
        }
    }
}

///Object field (property) assignment.
pub enum PropertyAssignment {
    ///A regular assignment.
    Assignment(PropertyName, Expression),

    ///A property getter.
    Getter(PropertyName, FunctionBody),

    ///A property setter.
    Setter(PropertyName, Identifier, FunctionBody),
}

impl Print for PropertyAssignment {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            PropertyAssignment::Assignment(ref name, ref expr) => {
                try!(name.print_with(f));
                try!(f.write(": "));
                expr.print_with(f)
            },
            PropertyAssignment::Getter(ref name, ref body) => {
                try!(f.write("get "));
                try!(name.print_with(f));
                try!(f.write("() "));
                body.print_with(f)
            },
            PropertyAssignment::Setter(ref name, ref arg, ref body) => {
                try!(f.write("set "));
                try!(name.print_with(f));
                try!(write!(f, "({}) ", arg));
                body.print_with(f)
            },
        }
    }
}

///An object field (property) name.
pub enum PropertyName {
    ///An identifier.
    Identifier(Identifier),

    ///A string.
    StringLiteral(StringLiteral),

    ///A number.
    NumericLiteral(NumericLiteral),
}

impl Print for PropertyName {
    #[inline]
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            PropertyName::Identifier(ref ident) => f.write(ident),
            PropertyName::StringLiteral(ref s) => s.print_with(f),
            PropertyName::NumericLiteral(ref n) => n.print_with(f),
        }
    }
}

///A general expression.
pub enum Expression {
    ///A left hand side expression.
    Lhs(LhsExpression),

    ///Apply a unary operator.
    Unary(UnaryOp, LhsExpression),

    ///Apply a binary operator.
    Binary(BinaryOp, Vec<Expression>),

    ///`...? ...: ...`.
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),

    ///Assign a value to something.
    Assignment(LhsExpression, Option<AssignmentOp>, Box<Expression>),
}

impl Expression {
    ///Build a new expression.
    pub fn build() -> ExpressionBuilder<()> {
        ExpressionBuilder::from(())
    }
}

impl Print for Expression {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            Expression::Lhs(ref expr) => expr.print_with(f),
            Expression::Unary(op, ref expr) => if op.is_postfix() {
                try!(expr.print_with(f));
                f.write(op)
            } else {
                try!(f.write(op));
                expr.print_with(f)
            },
            Expression::Binary(op, ref exprs) => {
                let mut first = true;
                for expr in exprs {
                    if !first {
                        try!(write!(f, " {} ", op));
                    }
                    try!(expr.print_with(f));
                    first = false;
                }
                Ok(())
            },
            Expression::Conditional(ref cond, ref if_true, ref if_false) => {
                try!(cond.print_with(f));
                try!(f.write('?'));
                try!(if_true.print_with(f));
                try!(f.write(':'));
                if_false.print_with(f)
            },
            Expression::Assignment(ref lhs, op, ref rhs) => {
                try!(lhs.print_with(f));
                if let Some(op) = op {
                    try!(write!(f, " {} ", op));
                } else {
                    try!(f.write(" = "));
                }
                rhs.print_with(f)
            },
        }
    }
}

#[derive(Copy, Clone)]
///Unary operator.
pub enum UnaryOp {
    ///Postfix increment.
    PostIncr,

    ///Postfix decrement.
    PostDecr,

    ///`delete ...`.
    Delete,

    ///`void ...`.
    Void,

    ///`typeof ...`.
    Typeof,

    ///Prefix increment.
    PreIncr,

    ///Prefix decrement.
    PreDecr,

    ///`+...`.
    Plus,

    ///Negation.
    Minus,

    ///Bitwise `not`.
    BitwiseNot,

    ///Logical `not`.
    LogicalNot,
}

impl UnaryOp {
    fn is_postfix(&self) -> bool {
        match *self {
            UnaryOp::PostIncr | UnaryOp::PostDecr => true,
            _ => false,
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnaryOp::PostIncr | UnaryOp::PreIncr => "++".fmt(f),
            UnaryOp::PostDecr | UnaryOp::PreDecr => "--".fmt(f),
            UnaryOp::Delete => "delete ".fmt(f),
            UnaryOp::Void => "void ".fmt(f),
            UnaryOp::Typeof => "typeof ".fmt(f),
            UnaryOp::Plus => '+'.fmt(f),
            UnaryOp::Minus => '-'.fmt(f),
            UnaryOp::BitwiseNot => '~'.fmt(f),
            UnaryOp::LogicalNot => '!'.fmt(f),
        }
    }
}

#[derive(Copy, Clone)]
///Binary operator.
pub enum BinaryOp {
    ///Multiplication.
    Mul,

    ///Division.
    Div,
    
    ///Modulo (remainder).
    Mod,
    
    ///Addition.
    Add,
    
    ///Subtraction.
    Sub,
    
    ///Left shift.
    Lsh,
    
    ///Right shift.
    Rsh,
    
    ///Unsigned right shift.
    Ursh,
    
    ///Less than.
    Lt,
    
    ///Greater than.
    Gt,
    
    ///Less than or equal.
    Leq,
    
    ///Greater than or equal.
    Geq,
    
    ///`... instanceof ...`.
    Instanceof,
    
    ///`... in ...`.
    In,
    
    ///Equal.
    Eq,
    
    ///Not equal.
    NotEq,
    
    ///Identity.
    Identical,
    
    ///Nonidentity.
    NotIdentical,
    
    ///Bitwise `and`.
    BitwiseAnd,
    
    ///Bitwise `xor`.
    BitwiseXor,
    
    ///Bitwise `or`.
    BitwiseOr,
    
    ///Logical `and`.
    LogicalAnd,
    
    ///Logical `or`.
    LogicalOr,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinaryOp::Mul => '*'.fmt(f),
            BinaryOp::Div => '/'.fmt(f),
            BinaryOp::Mod => '%'.fmt(f),
            BinaryOp::Add => '+'.fmt(f),
            BinaryOp::Sub => '-'.fmt(f),
            BinaryOp::Lsh => "<<".fmt(f),
            BinaryOp::Rsh => ">>".fmt(f),
            BinaryOp::Ursh => ">>>".fmt(f),
            BinaryOp::Lt => '<'.fmt(f),
            BinaryOp::Gt => '>'.fmt(f),
            BinaryOp::Leq => "<=".fmt(f),
            BinaryOp::Geq => ">=".fmt(f),
            BinaryOp::Eq => "==".fmt(f),
            BinaryOp::NotEq => "!=".fmt(f),
            BinaryOp::Identical => "===".fmt(f),
            BinaryOp::NotIdentical => "!==".fmt(f),
            BinaryOp::LogicalAnd => "&&".fmt(f),
            BinaryOp::LogicalOr => "||".fmt(f),
            BinaryOp::BitwiseAnd => '&'.fmt(f),
            BinaryOp::BitwiseXor => '^'.fmt(f),
            BinaryOp::BitwiseOr => '|'.fmt(f),
            BinaryOp::Instanceof => "instanceof".fmt(f),
            BinaryOp::In => "in".fmt(f),
        }
    }
}

#[derive(Copy, Clone)]
///An assignment operator.
pub enum AssignmentOp {
    ///Multiplication assignment.
    MulAssign,

    ///Division assignment.
    DivAssign,

    ///Modulo (remainder) assignment.
    ModAssign,

    ///Addition assignment.
    AddAssign,

    ///Subtraction assignment.
    SubAssign,
    
    ///Left shift assignment.
    LshAssign,
    
    ///Right shift assignment.
    RshAssign,
    
    ///Unsigned right shift assignment.
    UrshAssign,
    
    ///Bitwise `and` assignment.
    AndAssign,
    
    ///Bitwise `xor` assignment.
    XorAssign,
    
    ///Bitwise `or` assignment.
    OrAssign,
}

impl Display for AssignmentOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AssignmentOp::MulAssign => "*=".fmt(f),
            AssignmentOp::DivAssign => "/=".fmt(f),
            AssignmentOp::ModAssign => "%=".fmt(f),
            AssignmentOp::AddAssign => "+=".fmt(f),
            AssignmentOp::SubAssign => "-=".fmt(f),
            AssignmentOp::LshAssign => "<<=".fmt(f),
            AssignmentOp::RshAssign => ">>=".fmt(f),
            AssignmentOp::UrshAssign => ">>>=".fmt(f),
            AssignmentOp::AndAssign => "&=".fmt(f),
            AssignmentOp::XorAssign => "^=".fmt(f),
            AssignmentOp::OrAssign => "|=".fmt(f),
        }
    }
}

///A left hand side expression.
pub enum LhsExpression {
    ///A value expression.
    Value(ValueExpression),

    ///A function call.
    Call(CallExpression),
}

impl LhsExpression {
    ///Build a new left hand side expression.
    pub fn build() -> LhsExprBuilder<()> {
        LhsExprBuilder::from(())
    }
}

impl Print for LhsExpression {
    #[inline]
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            LhsExpression::Value(ref expr) => expr.print_with(f),
            LhsExpression::Call(ref expr) => expr.print_with(f),
        }
    }
}

///An expression that fetches a value.
pub enum ValueExpression {
    ///A path expression.
    Path(PathExpression),

    ///A constructor call without an argument list.
    New(Box<ValueExpression>),
}

impl ValueExpression {
    ///Build a new value expression.
    pub fn build() -> ValueExprBuilder<()> {
        ValueExprBuilder::from(())
    }
}

impl Print for ValueExpression {
    #[inline]
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            ValueExpression::Path(ref expr) => expr.print_with(f),
            ValueExpression::New(ref expr) => {
                try!(f.write("new "));
                expr.print_with(f)
            },
        }
    }
}

///A member access path expression.
pub struct PathExpression {
    ///The fist part of the path.
    pub base: PathBase,

    ///The following member access path.
    pub access: Vec<Access>,
}

impl PathExpression {
    ///Build a new path expression.
    pub fn build() -> PathBaseBuilder<()> {
        PathBaseBuilder::from(())
    }
}

impl Print for PathExpression {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        let add_parens = self.base.is_function() && !self.access.is_empty();

        if add_parens { try!(f.write('(')); }
        try!(self.base.print_with(f));
        if add_parens { try!(f.write(')')); }
        for access in &self.access {
            try!(access.print_with(f))
        }

        Ok(())
    }
}

///The first part of a path expression.
pub enum PathBase {
    ///A primary expression.
    Primary(PrimaryExpression),

    ///A function expression.
    Function(Option<Identifier>, Vec<Identifier>, FunctionBody),

    ///A constructor call.
    New(Box<PathExpression>, Vec<Expression>),
}

impl PathBase {
    fn is_function(&self) -> bool {
        if let PathBase::Function(_, _, _) = *self {
            true
        } else {
            false
        }
    }
}

impl Print for PathBase {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            PathBase::Primary(ref expr) => expr.print_with(f),
            PathBase::Function(ref name, ref args, ref body) => {
                if let Some(ref name) = *name {
                    try!(write!(f, "function {}(", name));
                } else {
                    try!(f.write("function("));
                }
                let mut first = true;
                for arg in args {
                    if first {
                        try!(f.write(arg));
                    } else {
                        try!(write!(f, ", {}", arg));
                    }
                    first = false;
                }
                try!(f.write(") "));
                body.print_with(f)
            },
            PathBase::New(ref ctor, ref args) => {
                try!(f.write("new "));
                try!(ctor.print_with(f));
                try!(f.write("("));
                let mut first = true;
                for arg in args {
                    if !first {
                        try!(f.write(", "));
                    }
                    try!(arg.print_with(f));
                    first = false;
                }
                f.write(")")
            }
        }
    }
}

///A function call expression.
pub struct CallExpression {
    ///The path leading up to the function.
    pub base: CallBase,

    ///The function call arguments.
    pub arguments: Vec<Expression>,

    ///The following member access path.
    pub access: Vec<Access>,
}

impl Print for CallExpression {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        try!(self.base.print_with(f));
        try!(f.write('('));
        let mut first = true;
        for arg in &self.arguments {
            if !first {
                try!(f.write(", "));
            }
            try!(arg.print_with(f));
            first = false;
        }
        try!(f.write(')'));
        for access in &self.access {
            try!(access.print_with(f))
        }

        Ok(())
    }
}

///The base path of a function call.
pub enum CallBase {
    ///A path expression.
    Path(PathExpression),

    ///Another function call.
    Call(Box<CallExpression>),
}

impl Print for CallBase {
    #[inline]
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            CallBase::Path(ref expr) => expr.print_with(f),
            CallBase::Call(ref expr) => expr.print_with(f),
        }
    }
}

///A path access notation.
pub enum Access {
    ///Dot notation (`x.y`).
    Dot(Identifier),

    ///Bracket notation (`x[y]`).
    Bracket(Expressions),
}

impl Print for Access {
    #[inline]
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            Access::Dot(ref ident) => write!(f, ".{}", ident),
            Access::Bracket(ref expr) => {
                try!(f.write("["));
                try!(expr.print_with(f));
                f.write("]")
            },
        }
    }
}

///A statement.
pub enum Statement {
    ///Code block.
    Block(Block),

    ///Variable declaration.
    Variable(Vec<VarDecl>),

    ///An empty statement (just a `;`).
    Empty,

    ///An expression.
    Expression(Expressions),

    ///An `if` statement.
    If(Expressions, Box<Statement>, Option<Box<Statement>>),

    ///A `do ... while(...)` statement.
    Do(Box<Statement>, Expressions),

    ///A `while(...) ...` statement.
    While(Expressions, Box<Statement>),

    ///A `for(...; ...; ...) ...` statement.
    For(Expressions, Expressions, Expressions, Box<Statement>),

    ///A `for(var ...; ...; ...) ...` statement.
    ForVar(Vec<VarDecl>, Expressions, Expressions, Box<Statement>),

    ///A `for(... in ...) ...` statement.
    ForIn(LhsExpression, Expressions, Box<Statement>),

    ///A `for(var ... in ...) ...` statement.
    ForInVar(Identifier, Expressions, Box<Statement>),

    ///A `continue;` statement.
    Continue(Option<Identifier>),
    
    ///A `break ...;` statement.
    Break(Option<Identifier>),
    
    ///A `return ...;` statement.
    Return(Expressions),
    
    ///A `with(...) ...` statement.
    With(Expressions, Box<Statement>),
    
    ///A labeled statement.
    Labeled(Identifier, Box<Statement>),
    
    ///A `switch(...) { ... }` statement.
    Switch(Expressions, Vec<CaseClause>),
    
    ///A `throw ...;` statement.
    Throw(Expressions),

    ///A try-catch-finally statement.
    Try(Block, Option<Catch>, Block),

    ///A `debugger;` statement.
    Debugger,
}

impl Statement {
    ///Build a new statement.
    pub fn build() -> StatementBuilder<()> {
        StatementBuilder::from(())
    }
}

impl Print for Statement {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            Statement::Block(ref block) => {
                try!(block.print_with(f));
                f.new_line()
            },
            Statement::Variable(ref vars) => {
                try!(f.write("var "));
                let mut first = true;
                for var in vars {
                    if !first {
                        try!(f.write(", "));
                    }
                    
                    try!(f.write(&var.ident));
                    if let Some(ref init) = var.init {
                        try!(f.write(" = "));
                        try!(init.print_with(f));
                    }

                    first = false;
                }
                try!(f.write(';'));
                f.new_line()
            },
            Statement::Empty => {
                try!(f.write(';'));
                f.new_line()
            },
            Statement::Expression(ref expr) => {
                try!(expr.print_with(f));
                try!(f.write(';'));
                f.new_line()
            },
            Statement::If(ref cond, ref if_true, ref if_false) => {
                try!(f.write("if("));
                try!(cond.print_with(f));
                try!(f.write(") "));
                try!(if_true.print_with(f));
                if let Some(ref if_false) = *if_false {
                    try!(f.write(" else "));
                    try!(if_false.print_with(f));
                }
                Ok(())
            },
            Statement::Do(ref stmt, ref cond) => {
                try!(f.write("do "));
                try!(stmt.print_with(f));
                try!(f.write(" while("));
                try!(cond.print_with(f));
                try!(f.write(");"));
                f.new_line()
            },
            Statement::While(ref cond, ref stmt) => {
                try!(f.write("while("));
                try!(cond.print_with(f));
                try!(f.write(") "));
                stmt.print_with(f)
            },
            Statement::For(ref init, ref cond, ref incr, ref stmt) => {
                try!(f.write("for("));
                try!(init.print_with(f));
                try!(f.write(';'));
                try!(cond.print_with(f));
                try!(f.write(';'));
                try!(incr.print_with(f));
                try!(f.write(") "));
                stmt.print_with(f)
            },
            Statement::ForVar(ref vars, ref cond, ref incr, ref stmt) => {
                try!(f.write("for(var "));
                let mut first = true;
                for var in vars {
                    if !first {
                        try!(f.write(", "));
                    }
                    
                    try!(f.write(&var.ident));
                    if let Some(ref init) = var.init {
                        try!(f.write(" = "));
                        try!(init.print_with(f));
                    }

                    first = false;
                }
                try!(f.write(';'));
                try!(cond.print_with(f));
                try!(f.write(';'));
                try!(incr.print_with(f));
                try!(f.write(") "));
                stmt.print_with(f)
            },
            Statement::ForIn(ref lhs, ref rhs, ref stmt) => {
                try!(f.write("for("));
                try!(lhs.print_with(f));
                try!(f.write(" in "));
                try!(rhs.print_with(f));
                try!(f.write(") "));
                stmt.print_with(f)
            },
            Statement::ForInVar(ref var, ref rhs, ref stmt) => {
                try!(f.write("for(var "));
                try!(f.write(var));
                try!(f.write(" in "));
                try!(rhs.print_with(f));
                try!(f.write(") "));
                stmt.print_with(f)
            },
            Statement::Continue(ref label) => {
                if let Some(ref label) = *label {
                    try!(write!(f, "continue {};", label));
                } else {
                    try!(f.write("continue;"));
                }
                f.new_line()
            },
            Statement::Break(ref label) => {
                if let Some(ref label) = *label {
                    try!(write!(f, "break {};", label));
                } else {
                    try!(f.write("break;"));
                }
                f.new_line()
            },
            Statement::Return(ref expr) => {
                if !expr.exprs.is_empty() {
                    try!(f.write("return "));
                    try!(expr.print_with(f));
                    try!(f.write(';'));
                } else {
                    try!(f.write("return;"));
                }
                f.new_line()
            },
            Statement::With(ref expr, ref stmt) => {
                try!(f.write(" with("));
                try!(expr.print_with(f));
                try!(f.write(") "));
                stmt.print_with(f)
            },
            Statement::Labeled(ref label, ref stmt) => {
                try!(write!(f, "{}: ", label));
                stmt.print_with(f)
            },
            Statement::Switch(ref expr, ref cases) => {
                try!(f.write(" with("));
                try!(expr.print_with(f));
                try!(f.write(") {"));
                try!(f.new_line());

                {
                    let mut f = f.indented();
                    for clause in cases {
                        try!(clause.print_with(&mut f));
                        try!(f.new_line());
                    }
                }
                try!(f.write('}'));
                f.new_line()
            },
            Statement::Throw(ref expr) => {
                try!(f.write("throw "));
                try!(expr.print_with(f));
                try!(f.write(';'));
                f.new_line()
            },
            Statement::Try(ref block, ref catch, ref finally) => {
                try!(f.write("try "));
                try!(block.print_with(f));
                if let Some(ref catch) = *catch {
                    try!(write!(f, "catch ({}) ", catch.exception));
                    try!(catch.block.print_with(f));
                }
                if !finally.statements.is_empty() {
                    try!(f.write("finally "));
                    try!(finally.print_with(f));
                }
                f.new_line()
            },
            Statement::Debugger => {
                try!(f.write("debugger ;"));
                f.new_line()
            },
        }
    }
}

///A variable declaration.
pub struct VarDecl {
    ///The variable name.
    pub ident: Identifier,
    ///An optional initialization expression.
    pub init: Option<Expression>,
}

///A clause in a `switch` statement.
pub enum CaseClause {
    ///A regular `case x: ...` clause.
    Case(Expressions, Vec<Statement>),
    ///A `default: ...` clause.
    Default(Vec<Statement>),
}

impl Print for CaseClause {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        let stmts = match *self {
            CaseClause::Case(ref expr, ref stmts) => {
                try!(f.write("case "));
                try!(expr.print_with(f));
                try!(f.write(":"));
                stmts
            },
            CaseClause::Default(ref stmts) =>  {
                try!(f.write("default :"));
                stmts
            },
        };
        try!(f.new_line());

        {
            let mut f = f.indented();
            for stmt in stmts {
                try!(stmt.print_with(&mut f));
                try!(f.new_line());
            }
        }

        Ok(())
    }
}

///A catch statement.
pub struct Catch {
    ///The exception variable.
    pub exception: Identifier,
    ///What to do when the exception is caught.
    pub block: Block,
}

///A block of statements, surrounded with `{ ... }`.
pub struct Block {
    ///The statements inside the block.
    pub statements: Vec<Statement>,
}

impl Block {
    ///Build a new code block.
    pub fn build() -> BlockBuilder<()> {
        BlockBuilder::from(())
    }
}

impl Print for Block {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        try!(f.write("{"));
        try!(f.new_line());

        {
            let mut f = f.indented();

            for stmt in &self.statements {
                try!(stmt.print_with(&mut f));
            }
        }

        f.write("}")
    }
}

///The body of a function.
pub struct FunctionBody {
    ///The statements and functions that makes up function body.
    pub elements: Vec<SourceElement>,
}

impl FunctionBody {
    ///Build a new function body.
    pub fn build() -> FnBodyBuilder<()> {
        FnBodyBuilder::from(())
    }
}

impl Print for FunctionBody {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        try!(f.write("{"));
        try!(f.new_line());

        {
            let mut f = f.indented();

            for element in &self.elements {
                try!(element.print_with(&mut f));
            }
        }

        f.write("}")
    }
}

///Source elements makes up scripts and function bodies.
pub enum SourceElement {
    ///A statement.
    Statement(Statement),

    ///A function.
    Function(Identifier, Vec<Identifier>, FunctionBody)
}

impl SourceElement {
    ///Build a new source element.
    pub fn build() -> SourceElementBuilder<()> {
        SourceElementBuilder::from(())
    }
}

impl Print for SourceElement {
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()> {
        match *self {
            SourceElement::Statement(ref stmt) => stmt.print_with(f),
            SourceElement::Function(ref name, ref args, ref body) => {
                try!(write!(f, "function {}(", name));
                let mut first = true;
                for arg in args {
                    if first {
                        try!(f.write(arg));
                    } else {
                        try!(write!(f, ", {}", arg));
                    }
                    first = false;
                }
                try!(f.write(") "));
                try!(body.print_with(f));
                f.new_line()
            }
        }
    }
}
