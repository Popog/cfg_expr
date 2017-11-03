use std::str::{self, FromStr};
use std::fmt;
use std::error::Error;
use std::iter::Peekable;

use token::{Tokenizer,Token,TokenType,TokenError};

struct Parser<'a> {
    t: Peekable<Tokenizer<'a>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Cfg {
    Name(String),
    KeyPair(String, String),
}

#[derive(Clone, PartialEq, Eq,Debug)]
pub enum CfgExpr {
    Not(Box<CfgExpr>),
    All(Vec<CfgExpr>),
    Any(Vec<CfgExpr>),
    Value(Cfg),
}

impl FromStr for Cfg {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Cfg, ParseError> {
        let mut p = Parser::from(s);
        let e = p.cfg()?;
        match p.t.next() {
            Some(Ok(t)) => Err(ParseError::UnexpectedToken{
                expected: None,
                found: t.to_string(),
            }),
            Some(Err(e)) => Err(ParseError::BadToken(e)),
            None => Ok(e),
        }
    }
}

impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Cfg::Name(ref s) => s.fmt(f),
            Cfg::KeyPair(ref k, ref v) => write!(f, "{} = \"{}\"", k, v),
        }
    }
}

impl CfgExpr {
    pub fn matches<'a, I>(&self, cfgs: I) -> bool
    where I: Clone+IntoIterator<Item=&'a Cfg> {
        match *self {
            CfgExpr::Not(ref e) => !e.matches(cfgs),
            CfgExpr::All(ref e) => e.iter().all(|e| e.matches(cfgs.clone())),
            CfgExpr::Any(ref e) => e.iter().any(|e| e.matches(cfgs.clone())),
            CfgExpr::Value(ref e) => cfgs.into_iter().any(|cfg| cfg == e),
        }
    }
}

impl FromStr for CfgExpr {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<CfgExpr, ParseError> {
        let mut p = Parser::from(s);
        let e = p.expr()?;
        match p.t.next() {
            Some(Ok(t)) => Err(ParseError::UnexpectedToken{
                expected: None,
                found: t.to_string(),
            }),
            Some(Err(e)) => Err(ParseError::BadToken(e)),
            None => Ok(e),
        }
    }
}

impl fmt::Display for CfgExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        struct CommaSep<T: IntoIterator>(T);

        impl<T: IntoIterator+Clone> fmt::Display for CommaSep<T>
        where T::Item: fmt::Display {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let mut iter = self.0.clone().into_iter();
                if let Some(first) = iter.next() {
                    write!(f, "{}", first)?;
                    for v in iter {
                        write!(f, ", {}", v)?;
                    }
                }
                Ok(())
            }
        }

        match *self {
            CfgExpr::Not(ref e) => write!(f, "not({})", e),
            CfgExpr::All(ref e) => write!(f, "all({})", CommaSep(e)),
            CfgExpr::Any(ref e) => write!(f, "any({})", CommaSep(e)),
            CfgExpr::Value(ref e) => write!(f, "{}", e),
        }
    }
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(s: &'a str) -> Self {
        Parser {
            t: Tokenizer::from(s).peekable(),
        }
    }
}

impl<'a> Parser<'a> {
    fn expr(&mut self) -> Result<CfgExpr, ParseError> {
        match self.t.next() {
            Some(Ok(t)) => self.expr_helper(t),
            Some(Err(e)) => Err(ParseError::BadToken(e)),
            None => Err(ParseError::StringEmpty),
        }
    }

    fn expr_helper(&mut self, token: Token) -> Result<CfgExpr, ParseError> {
        match token {
            Token::Ident(op @ "all") |
            Token::Ident(op @ "any") => {
                let mut e = Vec::new();
                self.eat(TokenType::LeftParen)?;
                while !self.try(Token::RightParen) {
                    match self.t.next() {
                        Some(Ok(Token::RightParen)) => break,
                        Some(Ok(Token::Comma)) => {}
                        Some(Ok(t)) => e.push(self.expr_helper(t)?),
                        Some(Err(e)) => return Err(ParseError::BadToken(e)),
                        None => return Err(ParseError::MissingToken(TokenType::RightParen)),
                    }
                }
                if op == "all" {
                    Ok(CfgExpr::All(e))
                } else {
                    Ok(CfgExpr::Any(e))
                }
            },
            Token::Ident("not") => {
                self.eat(TokenType::LeftParen)?;
                let e = self.expr()?;
                self.eat(TokenType::RightParen)?;
                Ok(CfgExpr::Not(Box::new(e)))
            },
            t => self.cfg_helper(t).map(CfgExpr::Value),
        }

    }

    fn cfg(&mut self) -> Result<Cfg, ParseError> {
        match self.t.next() {
            Some(Ok(t)) => self.cfg_helper(t),
            Some(Err(e)) => Err(ParseError::BadToken(e)),
            None => Err(ParseError::StringEmpty),
        }
    }

    fn cfg_helper(&mut self, token: Token) -> Result<Cfg, ParseError> {
        match token {
            Token::Ident(name) => {
                let e = if self.try(Token::Equals) {
                    let val = match self.t.next() {
                        Some(Ok(Token::String(s))) => s,
                        Some(Ok(t)) => return Err(ParseError::UnexpectedToken{
                            expected: Some(TokenType::String),
                            found: t.to_string(),
                        }),
                        Some(Err(e)) => return Err(ParseError::BadToken(e)),
                        None => return Err(ParseError::MissingToken(TokenType::String)),
                    };
                    Cfg::KeyPair(name.to_string(), val.to_string())
                } else {
                    Cfg::Name(name.to_string())
                };
                Ok(e)
            },
            t => Err(ParseError::UnexpectedToken{
                expected: Some(TokenType::Ident),
                found: t.to_string(),
            }),
        }
    }

    fn try(&mut self, token: Token<'a>) -> bool {
        match self.t.peek() {
            Some(&Ok(ref t)) if token == *t => {}
            _ => return false,
        }
        self.t.next();
        true
    }

    fn eat(&mut self, token: TokenType) -> Result<(), ParseError> {
        match self.t.next() {
            Some(Ok(t)) => if t.is_type(token) {
                Ok(())
            } else {
                Err(ParseError::UnexpectedToken{
                    expected: Some(token),
                    found: t.to_string(),
                })
            },
            Some(Err(e)) => Err(ParseError::BadToken(e)),
            None => Err(ParseError::MissingToken(token)),
        }
    }
}


#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    BadToken(TokenError),
    UnexpectedToken{
        expected: Option<TokenType>,
        found: String,
    },
    MissingToken(TokenType),
    StringEmpty,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseError::BadToken(ref e) => write!(f, "Bad token: {}", e),
            &ParseError::UnexpectedToken{ref expected, ref found} => if let Some(expected) = expected.as_ref() {
                write!(f, "Expected `{}`, found `{}`", expected, found)
            } else {
                write!(f, "Expected end, found `{}`", found)
            },
            &ParseError::MissingToken(ref t) => write!(f, "Expected {}, found end", t),
            &ParseError::StringEmpty => write!(f, "Cannot parse from empty string"),
        }
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match self {
            &ParseError::BadToken(_) => "bad token",
            &ParseError::UnexpectedToken{..} => "unexpected token",
            &ParseError::MissingToken(_) => "missing token",
            &ParseError::StringEmpty => "cannot parse from empty string",
        }
    }

    fn cause(&self) -> Option<&Error> {
        match self {
            &ParseError::BadToken(ref e) => Some(e),
            &ParseError::UnexpectedToken{..} => None,
            &ParseError::MissingToken(_) => None,
            &ParseError::StringEmpty => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;
    use std::str::FromStr;

    use super::{Cfg, CfgExpr, ParseError};
    use token::TokenType;

    macro_rules! c {
        ($a:ident) => (
            Cfg::Name(stringify!($a).to_string())
        );
        ($a:ident = $e:expr) => (
            Cfg::KeyPair(stringify!($a).to_string(), $e.to_string())
        );
    }

    macro_rules! e {
        (any($($t:tt),*)) => (CfgExpr::Any(vec![$(e!($t)),*]));
        (all($($t:tt),*)) => (CfgExpr::All(vec![$(e!($t)),*]));
        (not($($t:tt)*)) => (CfgExpr::Not(Box::new(e!($($t)*))));
        (($($t:tt)*)) => (e!($($t)*));
        ($($t:tt)*) => (CfgExpr::Value(c!($($t)*)));
    }

    fn good<T>(s: &str, expected: T)
        where T: FromStr + PartialEq + fmt::Debug,
            T::Err: PartialEq + fmt::Debug
    {
        assert_eq!(T::from_str(s), Ok(expected));
    }
    
    fn bad<T>(s: &str, err: ParseError)
        where T: FromStr<Err=ParseError> + fmt::Display
    {
        match T::from_str(s) {
            Ok(cfg) => panic!("expected `{}` to not parse but got {}", s, cfg),
            Err(e) => assert_eq!(e, err),
        }
    }

    #[test]
    fn cfg_syntax() {
        good("foo", c!(foo));
        good("_bar", c!(_bar));
        good(" foo", c!(foo));
        good(" foo  ", c!(foo));
        good(r#" foo  = "bar""#, c!(foo = "bar"));
        good(r#"foo="""#, c!(foo = ""));
        good(r#" foo="3"      "#, c!(foo = "3"));
        good(r#"foo = "3 e""#, c!(foo = "3 e"));
    }

    #[test]
    fn cfg_syntax_bad() {
        use parse::ParseError::*;
        use token::TokenError::*;
        bad::<Cfg>("", StringEmpty);
        bad::<Cfg>(" ", StringEmpty);
        bad::<Cfg>("\t", BadToken(UnexpectedCharacter{pos:0, ch:'\t'}));
        bad::<Cfg>("7", BadToken(UnexpectedCharacter{pos:0, ch:'7'}));
        bad::<Cfg>("=", UnexpectedToken{expected: Some(TokenType::Ident), found: "`=`".to_string()});
        bad::<Cfg>(",", UnexpectedToken{expected: Some(TokenType::Ident), found: "`,`".to_string()});
        bad::<Cfg>("(", UnexpectedToken{expected: Some(TokenType::Ident), found: "`(`".to_string()});
        bad::<Cfg>("foo (", UnexpectedToken{expected: None, found: "`(`".to_string()});
        bad::<Cfg>("bar =", MissingToken(TokenType::String));
        bad::<Cfg>("bar = \"", BadToken(UnterminatedString{pos:6}));
        bad::<Cfg>("foo, bar", UnexpectedToken{expected: None, found: "`,`".to_string()});
    }

    #[test]
    fn cfg_expr() {
        good("foo", e!(foo));
        good("_bar", e!(_bar));
        good(" foo", e!(foo));
        good(" foo  ", e!(foo));
        good(r#" foo  = "bar""#, e!(foo = "bar"));
        good(r#"foo="""#, e!(foo = ""));
        good(r#" foo="3"      "#, e!(foo = "3"));
        good(r#"foo = "3 e""#, e!(foo = "3 e"));

        good("all()", e!(all()));
        good("all(a)", e!(all(a)));
        good("all(a, b)", e!(all(a, b)));
        good("all(a, )", e!(all(a)));
        good(r#"not(a = "b")"#, e!(not(a = "b")));
        good("not(all(a))", e!(not(all(a))));
    }

    #[test]
    fn cfg_expr_bad() {
        use parse::ParseError::*;
        use token::TokenError::*;
        bad::<CfgExpr>(" ", StringEmpty);
        bad::<CfgExpr>("\t", BadToken(UnexpectedCharacter{pos:0, ch:'\t'}));
        bad::<CfgExpr>("7", BadToken(UnexpectedCharacter{pos:0, ch:'7'}));
        bad::<CfgExpr>(" all", MissingToken(TokenType::LeftParen));
        bad::<CfgExpr>("all(a", MissingToken(TokenType::RightParen));
        bad::<CfgExpr>("not", MissingToken(TokenType::LeftParen));
        bad::<CfgExpr>("not(a", MissingToken(TokenType::RightParen));
        bad::<CfgExpr>("a =", MissingToken(TokenType::String));
        bad::<CfgExpr>("all(not())", UnexpectedToken{expected: Some(TokenType::Ident), found: "`)`".to_string()});
        bad::<CfgExpr>("foo(a)", UnexpectedToken{expected: None, found: "`(`".to_string()});
    }

    #[test]
    fn cfg_matches() {
        assert!(e!(foo).matches([c!(bar), c!(foo), c!(baz)]));
        assert!(e!(any(foo)).matches(&[c!(bar), c!(foo), c!(baz)]));
        assert!(e!(any(foo, bar)).matches(&[c!(bar)]));
        assert!(e!(any(foo, bar)).matches(&[c!(foo)]));
        assert!(e!(all(foo, bar)).matches(&[c!(foo), c!(bar)]));
        assert!(e!(all(foo, bar)).matches(&[c!(foo), c!(bar)]));
        assert!(e!(not(foo)).matches(&[c!(bar)]));
        assert!(e!(not(foo)).matches(&[]));
        assert!(e!(any((not(foo)), (all(foo, bar)))).matches(&[c!(bar)]));
        assert!(e!(any((not(foo)), (all(foo, bar)))).matches(&[c!(foo), c!(bar)]));

        assert!(!e!(foo).matches(&[]));
        assert!(!e!(foo="bar").matches(&[c!(foo)]));
        assert!(!e!(foo).matches(&[c!(foo="bar")]));
        assert!(!e!(foo).matches(&[c!(bar)]));
        assert!(!e!(foo).matches(&[c!(fo)]));
        assert!(!e!(any(foo)).matches(&[]));
        assert!(!e!(any(foo)).matches(&[c!(bar)]));
        assert!(!e!(any(foo)).matches(&[c!(bar), c!(baz)]));
        assert!(!e!(all(foo)).matches(&[c!(bar), c!(baz)]));
        assert!(!e!(all(foo, bar)).matches(&[c!(bar)]));
        assert!(!e!(all(foo, bar)).matches(&[c!(foo)]));
        assert!(!e!(all(foo, bar)).matches(&[]));
        assert!(!e!(not(bar)).matches(&[c!(bar)]));
        assert!(!e!(not(bar)).matches(&[c!(baz), c!(bar)]));
        assert!(!e!(any((not(foo)), (all(foo, bar)))).matches(&[c!(foo)]));
    }

}
