//!Code printing and formatting utilities.

use std::io::{self, Write};
use std::fmt::{Display, Arguments};
use std::ops::{Deref, DerefMut, Drop};

///A trait for printing the AST to a writer.
pub trait Print {
    ///Print using a formatter.
    fn print_with<W: Write>(&self, f: &mut Formatter<W>) -> io::Result<()>;

    ///Print with somewhat pretty formatting.
    fn pretty_print<W: Write>(&self, writer: W) -> io::Result<()> {
        self.print_with(&mut Formatter::new(writer, true))
    }

    ///Print with less whitespace.
    fn compact_print<W: Write>(&self, writer: W) -> io::Result<()> {
        self.print_with(&mut Formatter::new(writer, false))
    }
}

///Code formatter.
pub struct Formatter<W: Write> {
    writer: W,
    pretty: bool,
    indentation: Vec<u8>,
    empty_line: bool,
}

impl<W: Write> Formatter<W> {
    ///Create a new formatter from a writer.
    pub fn new(writer: W, pretty: bool) -> Formatter<W> {
        Formatter {
            writer: writer,
            pretty: pretty,
            indentation: vec![],
            empty_line: true,
        }
    }

    ///Start a new line if pretty printing is enabled.
    pub fn new_line(&mut self) -> io::Result<()> {
        self.empty_line = true;
        if self.pretty {
            writeln!(self.writer, "")
        } else {
            Ok(())
        }
    }

    ///Write something to the writer.
    pub fn write<C: Display>(&mut self, content: C) -> io::Result<()> {
        if self.empty_line && self.pretty {
            try!(self.writer.write_all(&self.indentation));
            self.empty_line = false;
        }

        write!(self.writer, "{}", content)
    }

    ///Create an indented formatter.
    pub fn indented(&mut self) -> Indented<W> {
        self.indentation.push(b'\t');
        Indented(self)
    }

    ///Write something formatted.
    pub fn write_fmt(&mut self, args: Arguments) -> io::Result<()> {
        if self.empty_line && self.pretty {
            try!(self.writer.write_all(&self.indentation));
            self.empty_line = false;
        }

        self.writer.write_fmt(args)
    }
}

///An indented formatter.
pub struct Indented<'a, W: Write + 'a>(&'a mut Formatter<W>);

impl<'a, W: Write> Deref for Indented<'a, W> {
    type Target = Formatter<W>;

    fn deref(&self) -> &Formatter<W> {
        self.0
    }
}

impl<'a, W: Write> DerefMut for Indented<'a, W> {
    fn deref_mut(&mut self) -> &mut Formatter<W> {
        self.0
    }
}

impl<'a, W: Write> Drop for Indented<'a, W> {
    fn drop(&mut self) {
        self.0.indentation.pop();
    }
}
