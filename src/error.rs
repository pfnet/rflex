use crate::scanner::{CharClassParseError, TranslateError};
use failure::{Backtrace, Context, Fail};
use std::fmt;
use std::fmt::Display;

#[derive(Fail, Debug)]
pub enum ErrorKind {
    #[fail(display = "IO error")]
    Io { error: std::io::Error },
    #[fail(display = "DSL parse error: {} in {}", message, position)]
    DSLParse { position: String, message: String },
    #[fail(display = "Regex char class parse error: {} line {}", error, line)]
    RegexCharClass {
        error: CharClassParseError,
        line: usize,
    },
    #[fail(display = "Regex translate error: {} line {}", error, line)]
    RegexTranslate { error: TranslateError, line: usize },
}

#[derive(Debug)]
pub struct Error {
    inner: Context<ErrorKind>,
}

impl Fail for Error {
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Error {
    pub fn new(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }

    pub fn kind(&self) -> &ErrorKind {
        self.inner.get_context()
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error {
            inner: Context::new(kind),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Error {
        Error {
            inner: Context::new(ErrorKind::Io { error }),
        }
    }
}

impl From<CharClassParseError> for Error {
    fn from(error: CharClassParseError) -> Error {
        Error {
            inner: Context::new(ErrorKind::RegexCharClass {
                error,
                line: 0usize,
            }),
        }
    }
}

impl From<Context<ErrorKind>> for Error {
    fn from(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }
}
