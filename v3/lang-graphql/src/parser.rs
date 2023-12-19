use std::{fmt::Display, str::FromStr};
use thiserror::Error;

use crate::{ast::spanning::*, lexer};

use super::ast::common::Name;

mod executable;
mod fragment;
mod schema;
mod selection_set;
mod value;

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    next_token: Option<lexer::Result>,
}

#[derive(Error, Debug, PartialEq, Clone)]
pub struct Error {
    expected_tokens: &'static [ExpectedToken],
    found: TokenFound,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expected_tokens = self
            .expected_tokens
            .iter()
            .fold(String::new(), |acc, expected_token| {
                acc + &expected_token.to_string() + ", "
            });
        let found = &self.found;
        write!(
            f,
            "expected one of {expected_tokens}, but encountered: {found}"
        )
    }
}

impl Error {
    pub fn new(expected_tokens: &'static [ExpectedToken], found: TokenFound) -> Self {
        Error {
            expected_tokens,
            found,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenFound {
    EndOfFile,
    LexerError(lexer::Error),
    Token(lexer::Token),
}

impl Display for TokenFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenFound::EndOfFile => write!(f, "end of file"),
            TokenFound::LexerError(e) => write!(f, "lexer error: {e}"),
            TokenFound::Token(token) => write!(f, "token: {token}"),
        }
    }
}

// This is the 'kind' of a token
#[derive(Debug, PartialEq, Clone)]
pub enum ExpectedToken {
    Keyword(Keyword),
    Punctuation(lexer::Punctuation),
    Name,
    Number,
    String,
}

impl Display for ExpectedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedToken::Keyword(keyword) => write!(f, "Keyword({keyword}"),
            ExpectedToken::Punctuation(p) => write!(f, "Punctuation({p}"),
            ExpectedToken::Name => "Name".fmt(f),
            ExpectedToken::Number => "Number".fmt(f),
            ExpectedToken::String => "String".fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    True,
    False,
    Null,
    Fragment,
    On,
    Query,
    Mutation,
    Subscription,
    Type,
    Input,
    Enum,
    Scalar,
    Implements,
    Interface,
    Union,
    Schema,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl FromStr for Keyword {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "null" => Ok(Keyword::Null),
            "fragment" => Ok(Keyword::Fragment),
            "on" => Ok(Keyword::On),
            "query" => Ok(Keyword::Query),
            "mutation" => Ok(Keyword::Mutation),
            "subscription" => Ok(Keyword::Subscription),
            "type" => Ok(Keyword::Type),
            "input" => Ok(Keyword::Input),
            "enum" => Ok(Keyword::Enum),
            "scalar" => Ok(Keyword::Scalar),
            "implements" => Ok(Keyword::Implements),
            "interface" => Ok(Keyword::Interface),
            "union" => Ok(Keyword::Union),
            "schema" => Ok(Keyword::Schema),
            _ => Err(()),
        }
    }
}

impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Null => "null",
            Keyword::Fragment => "fragment",
            Keyword::On => "on",
            Keyword::Query => "query",
            Keyword::Mutation => "mutation",
            Keyword::Subscription => "subscription",
            Keyword::Type => "type",
            Keyword::Input => "input",
            Keyword::Enum => "enum",
            Keyword::Scalar => "scalar",
            Keyword::Interface => "interface",
            Keyword::Union => "union",
            Keyword::Schema => "schema",
            Keyword::Implements => "implements",
        }
    }
    pub fn expected_tokens(&self) -> &'static [ExpectedToken] {
        match self {
            Keyword::True => &[ExpectedToken::Keyword(Keyword::True)],
            Keyword::False => &[ExpectedToken::Keyword(Keyword::False)],
            Keyword::Null => &[ExpectedToken::Keyword(Keyword::Null)],
            Keyword::Fragment => &[ExpectedToken::Keyword(Keyword::Fragment)],
            Keyword::On => &[ExpectedToken::Keyword(Keyword::On)],
            Keyword::Query => &[ExpectedToken::Keyword(Keyword::Query)],
            Keyword::Mutation => &[ExpectedToken::Keyword(Keyword::Mutation)],
            Keyword::Subscription => &[ExpectedToken::Keyword(Keyword::Subscription)],
            Keyword::Type => &[ExpectedToken::Keyword(Keyword::Type)],
            Keyword::Input => &[ExpectedToken::Keyword(Keyword::Input)],
            Keyword::Enum => &[ExpectedToken::Keyword(Keyword::Enum)],
            Keyword::Scalar => &[ExpectedToken::Keyword(Keyword::Scalar)],
            Keyword::Implements => &[ExpectedToken::Keyword(Keyword::Implements)],
            Keyword::Interface => &[ExpectedToken::Keyword(Keyword::Interface)],
            Keyword::Union => &[ExpectedToken::Keyword(Keyword::Union)],
            Keyword::Schema => &[ExpectedToken::Keyword(Keyword::Schema)],
        }
    }
}

impl Name {
    pub fn is_keyword(&self, keyword: &Keyword) -> bool {
        matches!(self, name if name.get() == keyword.as_str())
    }
}

impl lexer::Punctuation {
    pub fn expected_tokens(&self) -> &'static [ExpectedToken] {
        match self {
            lexer::Punctuation::Bang => &[ExpectedToken::Punctuation(lexer::Punctuation::Bang)],
            lexer::Punctuation::Dollar => &[ExpectedToken::Punctuation(lexer::Punctuation::Dollar)],
            lexer::Punctuation::Amp => &[ExpectedToken::Punctuation(lexer::Punctuation::Amp)],
            lexer::Punctuation::ParenL => &[ExpectedToken::Punctuation(lexer::Punctuation::ParenL)],
            lexer::Punctuation::ParenR => &[ExpectedToken::Punctuation(lexer::Punctuation::ParenR)],
            lexer::Punctuation::Spread => &[ExpectedToken::Punctuation(lexer::Punctuation::Spread)],
            lexer::Punctuation::Colon => &[ExpectedToken::Punctuation(lexer::Punctuation::Colon)],
            lexer::Punctuation::Equals => &[ExpectedToken::Punctuation(lexer::Punctuation::Equals)],
            lexer::Punctuation::At => &[ExpectedToken::Punctuation(lexer::Punctuation::At)],
            lexer::Punctuation::BracketL => {
                &[ExpectedToken::Punctuation(lexer::Punctuation::BracketL)]
            }
            lexer::Punctuation::BracketR => {
                &[ExpectedToken::Punctuation(lexer::Punctuation::BracketR)]
            }
            lexer::Punctuation::BraceL => &[ExpectedToken::Punctuation(lexer::Punctuation::BraceL)],
            lexer::Punctuation::Pipe => &[ExpectedToken::Punctuation(lexer::Punctuation::Pipe)],
            lexer::Punctuation::BraceR => &[ExpectedToken::Punctuation(lexer::Punctuation::BraceR)],
        }
    }
}

pub type Result<T> = core::result::Result<T, Positioned<Error>>;

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let lexer = lexer::Lexer::new(source);
        let mut parser = Parser {
            lexer,
            next_token: None,
        };
        parser.next_token();
        parser
    }

    pub fn peek(&self) -> &Option<lexer::Result> {
        &self.next_token
    }

    pub fn is_next_token(&self, expected: &lexer::Token) -> bool {
        match self.peek() {
            None => false,
            Some(Err(_)) => false,
            Some(Ok(token)) => token.item == *expected,
        }
    }

    pub fn is_next_token_keyword(&self, expected: &Keyword) -> bool {
        if let Some(Ok(token)) = self.peek() {
            matches!(
                &token.item,
                lexer::Token::Name(name) if name.get() == expected.as_str()
            )
        } else {
            false
        }
    }

    fn next_token(&mut self) -> Option<lexer::Result> {
        let current_token = self.next_token.take();
        self.next_token = self.lexer.read_next_token();
        current_token
    }

    fn eof<T>(&self, expected_tokens: &'static [ExpectedToken]) -> Result<T> {
        Err(Positioned::new(
            &self.lexer.get_position(),
            Error::new(expected_tokens, TokenFound::EndOfFile),
        ))
    }

    fn lexer_error<T>(
        error: &Positioned<lexer::Error>,
        expected_tokens: &'static [ExpectedToken],
    ) -> Result<T> {
        Err(Positioned::new(
            &error.position,
            Error::new(expected_tokens, TokenFound::LexerError(error.item.clone())),
        ))
    }

    fn unexpected_token<T>(
        expected: &'static [ExpectedToken],
        found: lexer::Token,
        location: &SourcePosition,
    ) -> Result<T> {
        Err(Positioned::new(
            location,
            Error::new(expected, TokenFound::Token(found)),
        ))
    }

    #[inline(always)]
    pub fn peek_fail(
        &self,
        expected_tokens: &'static [ExpectedToken],
    ) -> Result<&Spanning<lexer::Token>> {
        match self.peek() {
            None => self.eof(expected_tokens),
            Some(Err(error)) => Self::lexer_error(error, expected_tokens),
            Some(Ok(token)) => Ok(token),
        }
    }

    pub fn parse_token<T, F>(
        &mut self,
        expected_tokens: &'static [ExpectedToken],
        parse: F,
    ) -> Result<T>
    where
        F: Fn(Spanning<lexer::Token>) -> Result<T>,
    {
        match self.next_token() {
            None => self.eof(expected_tokens),
            Some(Err(error)) => Self::lexer_error(&error, expected_tokens),
            Some(Ok(token)) => parse(token),
        }
    }

    fn parse_keyword(&mut self, keyword: &Keyword) -> Result<Spanning<Name>> {
        let expected = keyword.expected_tokens();
        self.parse_token(expected, |token| match token.item {
            lexer::Token::Name(name) if name.get() == keyword.as_str() => {
                Ok(Spanning::start_end(token.start, token.end, name))
            }
            _ => Self::unexpected_token(expected, token.item, &token.start),
        })
    }

    fn parse_name(&mut self) -> Result<Spanning<Name>> {
        static EXPECTED: &[ExpectedToken] = &[ExpectedToken::Name];
        self.parse_token(EXPECTED, |token| match token.item {
            lexer::Token::Name(name) => Ok(Spanning::start_end(token.start, token.end, name)),
            _ => Self::unexpected_token(EXPECTED, token.item, &token.start),
        })
    }

    fn parse_punctuation(
        &mut self,
        punctuation: lexer::Punctuation,
    ) -> Result<Spanning<lexer::Punctuation>> {
        let expected = punctuation.expected_tokens();
        self.parse_token(expected, |token| match token.item {
            lexer::Token::Punctuation(found) if punctuation == found => {
                Ok(Spanning::start_end(token.start, token.end, found))
            }
            _ => Self::unexpected_token(expected, token.item, &token.start),
        })
    }

    // TODO: Ideally, this should return Option<Spanning<NonEmpty<T>>>
    fn parse_list<T, F1, F2>(&mut self, peek: F1, parse: F2) -> Result<Vec<T>>
    where
        F1: Fn(&Self) -> bool,
        F2: Fn(&mut Self) -> Result<T>,
    {
        let mut items = vec![];
        while peek(self) {
            items.push(parse(self)?);
        }
        Ok(items)
    }

    /// Parse delimited items into a `List`
    /// <start> <item>* <end>
    fn parse_delimited_list<T, F>(
        &mut self,
        start_token: lexer::Punctuation,
        end_token: lexer::Punctuation,
        parse: F,
    ) -> Result<Spanning<Vec<T>>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let start = self.parse_punctuation(start_token)?;
        let mut items = vec![];
        let end_token_ = &lexer::Token::Punctuation(end_token.clone());
        while !self.is_next_token(end_token_) {
            items.push(parse(self)?);
        }
        let end = self.parse_punctuation(end_token)?;
        Ok(Spanning::start_end(start.start, end.end, items))
    }

    /// Parse optional delimited items into a `List`
    /// (<start> <item>* <end>)?
    fn parse_optional_delimited_list<T, F>(
        &mut self,
        start_token: lexer::Punctuation,
        end_token: lexer::Punctuation,
        parse: F,
    ) -> Result<Option<Spanning<Vec<T>>>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        if self.is_next_token(&lexer::Token::Punctuation(start_token.clone())) {
            Ok(Some(self.parse_delimited_list(
                start_token,
                end_token,
                parse,
            )?))
        } else {
            Ok(None)
        }
    }

    fn parse_optional<T, F1, F2>(&mut self, peek: F1, parse: F2) -> Result<Option<T>>
    where
        F1: Fn(&lexer::Token) -> bool,
        F2: Fn(&mut Self) -> Result<T>,
    {
        if let Some(Ok(token)) = self.peek() {
            if peek(&token.item) {
                Ok(Some(parse(self)?))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}
