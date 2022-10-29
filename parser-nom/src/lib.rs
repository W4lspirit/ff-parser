pub mod shunting_yard;
pub mod tokenizer;

pub use shunting_yard::RPNError;
pub use tokenizer::TokenParseError;

use std::fmt;

/// An error produced during parsing or evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnknownVariable(String),

    /// An error returned by the parser.
    ParseError(TokenParseError),
    /// The shunting-yard algorithm returned an error.
    RPNError(RPNError),
    // A catch all for all other errors during evaluation
    EvalError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::UnknownVariable(ref name) => {
                write!(f, "Evaluation error: unknown variable `{}`.", name)
            }

            Error::ParseError(ref e) => {
                write!(f, "Parse error: ")?;
                e.fmt(f)
            }
            Error::RPNError(ref e) => {
                write!(f, "RPN error: ")?;
                e.fmt(f)
            }
            Error::EvalError(ref e) => {
                write!(f, "Eval error: ")?;
                e.fmt(f)
            }
        }
    }
}

impl From<TokenParseError> for Error {
    fn from(err: TokenParseError) -> Error {
        Error::ParseError(err)
    }
}

impl From<RPNError> for Error {
    fn from(err: RPNError) -> Error {
        Error::RPNError(err)
    }
}

impl std::error::Error for Error {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match *self {
            Error::ParseError(ref e) => Some(e),
            Error::RPNError(ref e) => Some(e),
            _ => None,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::shunting_yard::to_rpn;
    use crate::tokenizer::tokenize;
    use crate::tokenizer::Operation::*;
    use crate::tokenizer::Token::*;
    #[test]
    fn feature() {
        let k =
            tokenize("-sin(\"pi\" * 3)^ cos(2) / Func2(\"x\", f(\"y\"), \"z\") * _buildIN(\"y\")")
                .unwrap();
        dbg!(to_rpn(&k));

        assert_eq!(
            to_rpn(&k),
            Ok(vec![
                Unary(Minus),
                Func("sin".into(), None),
                Var("pi".into()),
                Binary(Times),
                Number(3.0),
                RParen,
                Binary(Pow),
                Func("cos".into(), None),
                Number(2.0),
                RParen,
                Binary(Div),
                Func("Func2".into(), None),
                Var("x".into()),
                Comma,
                Func("f".into(), None),
                Var("y".into()),
                RParen,
                Comma,
                Var("z".into()),
                RParen,
                Binary(Times),
                Func("_buildIN".into(), None),
                Var("y".into()),
                RParen
            ])
        );
    }
}
