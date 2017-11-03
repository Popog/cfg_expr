use std::error::Error;
use std::fmt;
use std::str;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    LeftParen,
    RightParen,
    Ident(&'a str),
    Comma,
    Equals,
    String(&'a str),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Ident,
    Comma,
    Equals,
    String,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::Ident => write!(f, "identifier"),
            TokenType::String => write!(f, "string"),
            TokenType::LeftParen => write!(f, "`(`"),
            TokenType::RightParen => write!(f, "`)`"),
            TokenType::Comma => write!(f, "`,`"),
            TokenType::Equals => write!(f, "`=`"),
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Token::Ident(ref s) => write!(f, "`{}`", s),
            &Token::String(ref s) => write!(f, r#"`"{}"`"#, s),
            t => t.get_type().fmt(f),
        }
    }
}

impl<'a> Token<'a> {
    pub fn is_type(&self, t: TokenType) -> bool {
        self.get_type() == t
    }
    
    fn get_type(&self) -> TokenType {
        match self {
            &Token::LeftParen => TokenType::LeftParen,
            &Token::RightParen => TokenType::RightParen,
            &Token::Ident(_) => TokenType::Ident,
            &Token::Comma => TokenType::Comma,
            &Token::Equals => TokenType::Equals,
            &Token::String(_) => TokenType::String,
        }

    }
}

pub struct Tokenizer<'a> {
    s: Peekable<str::CharIndices<'a>>,
    orig: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenError {
    UnterminatedString{
        pos: usize,
    },
    UnexpectedCharacter{
        pos: usize,
        ch: char,
    },
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &TokenError::UnterminatedString{pos} => write!(f, "Unterminated string starting at {}", pos),
            &TokenError::UnexpectedCharacter{pos, ch} => write!(f, "Unexpected character {} at {}", ch, pos),
        }
    }
}

impl Error for TokenError {
    fn description(&self) -> &str {
        match self {
            &TokenError::UnterminatedString{..} => "unterminated string",
            &TokenError::UnexpectedCharacter{..} => "unexpected character",
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl<'a> From<&'a str> for Tokenizer<'a> {
    fn from(s: &'a str) -> Self {
        Tokenizer{
            s: s.char_indices().peekable(),
            orig: s,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, TokenError>;

    fn next(&mut self) -> Option<Result<Token<'a>, TokenError>> {
        loop {
            match self.s.next() {
                Some((_, ' ')) => {}
                Some((_, '(')) => return Some(Ok(Token::LeftParen)),
                Some((_, ')')) => return Some(Ok(Token::RightParen)),
                Some((_, ',')) => return Some(Ok(Token::Comma)),
                Some((_, '=')) => return Some(Ok(Token::Equals)),
                Some((start, '"')) => {
                    while let Some((end, ch)) = self.s.next() {
                        if ch == '"' {
                            return Some(Ok(Token::String(&self.orig[start+1..end])))
                        }
                    }
                    return Some(Err(TokenError::UnterminatedString{pos: start}))
                }
                Some((start, ch)) if is_ident_start(ch) => {
                    while let Some(&(end, ch)) = self.s.peek() {
                        if !is_ident_rest(ch) {
                            return Some(Ok(Token::Ident(&self.orig[start..end])))
                        } else {
                            self.s.next();
                        }
                    }
                    return Some(Ok(Token::Ident(&self.orig[start..])))
                }
                Some((pos, ch)) => return Some(Err(TokenError::UnexpectedCharacter{pos, ch})),
                None => return None
            }
        }
    }
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')
}

fn is_ident_rest(ch: char) -> bool {
    is_ident_start(ch) || ('0' <= ch && ch <= '9')
}

#[cfg(test)]
mod tests {
    use super::{Tokenizer, Token, TokenError};

    #[test]
    fn single_token() {
        let mut t = Tokenizer::from("(");
        assert_eq!(t.next(), Some(Ok(Token::LeftParen)));
        assert_eq!(t.next(), None);

        t = Tokenizer::from(")");
        assert_eq!(t.next(), Some(Ok(Token::RightParen)));
        assert_eq!(t.next(), None);

        t = Tokenizer::from(",");
        assert_eq!(t.next(), Some(Ok(Token::Comma)));
        assert_eq!(t.next(), None);

        t = Tokenizer::from("=");
        assert_eq!(t.next(), Some(Ok(Token::Equals)));
        assert_eq!(t.next(), None);

        t = Tokenizer::from(r#""foo""#);
        assert_eq!(t.next(), Some(Ok(Token::String("foo"))));
        assert_eq!(t.next(), None);

        t = Tokenizer::from("foo");
        assert_eq!(t.next(), Some(Ok(Token::Ident("foo"))));
        assert_eq!(t.next(), None);

        t = Tokenizer::from("_bar");
        assert_eq!(t.next(), Some(Ok(Token::Ident("_bar"))));
        assert_eq!(t.next(), None);
    }

    #[test]
    fn single_bad() {
        let mut t = Tokenizer::from("");
        assert_eq!(t.next(), None);
        
        t = Tokenizer::from(" ");
        assert_eq!(t.next(), None);

        t = Tokenizer::from("\t");
        assert_eq!(t.next(), Some(Err(TokenError::UnexpectedCharacter{pos:0, ch: '\t'})));
        assert_eq!(t.next(), None);

        t = Tokenizer::from(r#"" "#);
        assert_eq!(t.next(), Some(Err(TokenError::UnterminatedString{pos:0})));
        assert_eq!(t.next(), None);

        t = Tokenizer::from("7");
        assert_eq!(t.next(), Some(Err(TokenError::UnexpectedCharacter{pos:0, ch: '7'})));
        assert_eq!(t.next(), None);
    }
}