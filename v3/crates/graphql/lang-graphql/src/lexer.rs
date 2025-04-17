/// Lexer for both executable documents (i.e. graphql queries) and schema documents (SDL), which
/// share a common syntax
use std::fmt::Display;
use std::str::{FromStr, from_utf8_unchecked};
use std::{char, iter::Iterator};
use thiserror::Error;

use crate::ast::spanning::{Positioned, SourcePosition, Spanning};

use super::ast;

pub mod number;
pub mod string;

// const LUT: [u8; 256] = [
//     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
//     0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
//     0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
//     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
// ];

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    // !
    Bang,
    // $
    Dollar,
    // &
    Amp,
    // (
    ParenL,
    // )
    ParenR,
    // ...
    Spread,
    // :
    Colon,
    // =
    Equals,
    // @
    At,
    // [
    BracketL,
    // ]
    BracketR,
    // {
    BraceL,
    // |
    Pipe,
    // }
    BraceR,
}

impl Punctuation {
    pub fn as_str(&self) -> &'static str {
        match self {
            Punctuation::Bang => "!",
            Punctuation::Dollar => "$",
            Punctuation::Amp => "&",
            Punctuation::ParenL => "(",
            Punctuation::ParenR => ")",
            Punctuation::Spread => "...",
            Punctuation::Colon => ":",
            Punctuation::Equals => "=",
            Punctuation::At => "@",
            Punctuation::BracketL => "[",
            Punctuation::BracketR => "]",
            Punctuation::BraceL => "{",
            Punctuation::BraceR => "}",
            Punctuation::Pipe => "|",
        }
    }
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Punctuation(Punctuation),
    // A Name token
    Name(ast::common::Name),
    // Name(String),
    Number(number::NumberToken),
    // A String token
    String(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Punctuation(p) => write!(f, "Punctuation({p})"),
            Token::Name(name) => write!(f, "Name({name})"),
            Token::Number(number) => write!(f, "Number({number})"),
            Token::String(str) => write!(f, "String({str})"),
        }
    }
}

impl From<ast::common::Name> for Token {
    fn from(value: ast::common::Name) -> Self {
        Token::Name(value)
    }
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        Token::String(String::from(s))
    }
}

impl Token {
    pub fn is_punctuation(&self, expected: Punctuation) -> bool {
        matches!(self, Token::Punctuation(punctuation) if *punctuation == expected)
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    bytes: &'a [u8],
    ix: usize,
    line: usize,
    column: usize,
}

/// Error when tokenizing the input source
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum Error {
    /// An unexpected character was found
    ///
    /// Unexpected characters are characters that _do_ exist in the GraphQL
    /// language, but is not expected at the current position in the document.
    #[error("unexpected character in the document: {0:?}")]
    UnexpectedCharacter(char),

    /// The input source was unexpectedly terminated
    ///
    /// Emitted when the current token requires a succeeding character, but
    /// the source has reached EOF. Emitted when scanning e.g. `"1."`.
    #[error("end of file reached when expecting further input")]
    UnexpectedEndOfFile,

    /// An invalid string literal was found
    #[error("invalid string literal found: {0:?}")]
    InvalidString(string::Error),

    /// An invalid number literal was found
    #[error("invalid number literal found: {0:?}")]
    InvalidNumber(number::Error),

    // An invalid graphql name
    #[error("invalid graphql name: {0}")]
    InvalidGraphQlName(String),
}

impl From<ast::common::InvalidGraphQlName> for Error {
    fn from(error: ast::common::InvalidGraphQlName) -> Self {
        Error::InvalidGraphQlName(error.0)
    }
}

pub type Result = core::result::Result<Spanning<Token>, Positioned<Error>>;

#[inline]
fn consume_ascii_chars<F>(data: &[u8], mut f: F) -> usize
where
    F: FnMut(u8) -> bool,
{
    data.iter().take_while(|&&c| f(c)).count()
}

impl<'a> Lexer<'a> {
    #[doc(hidden)]
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            bytes: source.as_bytes(),
            ix: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn get_position(&self) -> SourcePosition {
        SourcePosition::new(self.line, self.column)
    }

    #[inline]
    fn seek_line(&mut self) {
        self.seek_lines(1);
    }

    #[inline]
    fn seek_lines(&mut self, count: usize) {
        if count > 0 {
            self.line += count;
            self.column = 1;
        }
    }

    #[inline]
    fn seek_column(&mut self) {
        self.seek_column_by(1);
    }

    #[inline]
    fn seek_column_by(&mut self, count: usize) {
        self.column += count;
    }

    #[inline]
    fn seek(&mut self, lines: usize, columns: usize) {
        self.seek_lines(lines);
        self.seek_column_by(columns);
    }

    #[inline]
    fn consume_punctuation_token(&mut self, t: Punctuation) -> Spanning<Token> {
        let token_position = SourcePosition::new(self.line, self.column);
        self.ix += 1;
        self.column += 1;
        Spanning::single_width(&token_position, Token::Punctuation(t))
    }

    pub fn read_next_token(&mut self) -> Option<Result> {
        // We have to loop here to get the next token as we have to
        // skip comments and whitespace and return the next token
        loop {
            match self.bytes.get(self.ix) {
                Some(&c) => match c {
                    // 'Name'
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => return Some(self.parse_name()),

                    b' ' | b'\t' | b'\n' | b',' | b'\r' => {
                        self.scan_whitespace();
                        continue;
                    }

                    // Integers and Floating point numbers
                    b'0'..=b'9' | b'-' => return Some(self.parse_number()),

                    // String values, both regular and block start with '"'
                    b'"' => return Some(self.parse_string()),

                    // A comment starts with '#'
                    b'#' => {
                        let comment_length = consume_ascii_chars(&self.bytes[self.ix..], |c| {
                            c != b'\r' && c != b'\n'
                        });
                        self.ix += comment_length;
                        continue;
                    }

                    // Punctuation
                    b'!' => return Some(Ok(self.consume_punctuation_token(Punctuation::Bang))),
                    b'$' => return Some(Ok(self.consume_punctuation_token(Punctuation::Dollar))),
                    b'&' => return Some(Ok(self.consume_punctuation_token(Punctuation::Amp))),
                    b'(' => return Some(Ok(self.consume_punctuation_token(Punctuation::ParenL))),
                    b')' => return Some(Ok(self.consume_punctuation_token(Punctuation::ParenR))),
                    b':' => return Some(Ok(self.consume_punctuation_token(Punctuation::Colon))),
                    b'=' => return Some(Ok(self.consume_punctuation_token(Punctuation::Equals))),
                    b'@' => return Some(Ok(self.consume_punctuation_token(Punctuation::At))),
                    b'[' => return Some(Ok(self.consume_punctuation_token(Punctuation::BracketL))),
                    b']' => return Some(Ok(self.consume_punctuation_token(Punctuation::BracketR))),
                    b'{' => return Some(Ok(self.consume_punctuation_token(Punctuation::BraceL))),
                    b'|' => return Some(Ok(self.consume_punctuation_token(Punctuation::Pipe))),
                    b'}' => return Some(Ok(self.consume_punctuation_token(Punctuation::BraceR))),
                    b'.' => return Some(self.parse_spread()),
                    _ => {
                        let parse_error = unsafe { from_utf8_unchecked(&self.bytes[self.ix..]) }
                            .chars()
                            .next()
                            .map_or(Error::UnexpectedEndOfFile, |c| {
                                Error::UnexpectedCharacter(c)
                            });
                        return Some(Err(Positioned::new(&self.get_position(), parse_error)));
                    }
                },
                None => {
                    return None;
                }
            }
        }
    }

    fn parse_spread(&mut self) -> Result {
        let count = consume_ascii_chars(&self.bytes[self.ix..], |c| c == b'.');
        if count >= 3 {
            self.ix += 3;
            self.seek_column_by(3);
            Ok(Spanning::start_end(
                SourcePosition::new(self.line, self.column - 3),
                SourcePosition::new(self.line, self.column - 1),
                Token::Punctuation(Punctuation::Spread),
            ))
        } else {
            self.ix += count;
            self.seek_column_by(count);
            let parse_error = unsafe { from_utf8_unchecked(&self.bytes[self.ix..]) }
                .chars()
                .next()
                .map_or(Error::UnexpectedEndOfFile, |c| {
                    Error::UnexpectedCharacter(c)
                });
            let position = self.get_position();
            Err(Positioned::new(&position, parse_error))
        }
    }

    // invariant: needs to be called after checking the first character
    fn parse_name(&mut self) -> Result {
        let start = self.ix;
        // let name_length = consume_ascii_chars(&self.bytes[self.ix..], |c| match c {
        //     b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => true,
        //     _ => false,
        // });
        let name_length = self.bytes[self.ix..]
            .iter()
            .take_while(|&&c| matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'))
            .count();
        // let name_length = self.bytes[self.ix..]
        //     .iter()
        //     .take_while(|&&c| LUT[c as usize] & 1 > 0 )
        //     .count();
        self.seek_column_by(name_length);
        self.ix += name_length;
        let name = unsafe { std::str::from_utf8_unchecked(&self.bytes[start..self.ix]) };

        let graphql_name = ast::common::Name::from_str(name)
            .map_err(|e| Positioned::new(&self.get_position(), e.into()))?;

        Ok(Spanning::start_end(
            SourcePosition::new(self.line, self.column - (self.ix - start)),
            SourcePosition::new(self.line, self.column - 1),
            Token::Name(graphql_name),
        ))
    }

    fn parse_string(&mut self) -> Result {
        match string::parse_string(&self.bytes[self.ix..]) {
            Ok((s, consumed, consumed_bytes)) => {
                self.ix += consumed_bytes;
                let start_position = self.get_position();
                self.seek(
                    consumed.line_breaks,
                    consumed.chars_without_further_line_break,
                );
                Ok(Spanning::start_end(
                    start_position,
                    // Since strings always end in a ", self.column will always be > 1.
                    SourcePosition::new(self.line, self.column - 1),
                    Token::String(s),
                ))
            }
            Err((e, consumed)) => {
                self.seek(
                    consumed.line_breaks,
                    consumed.chars_without_further_line_break,
                );
                Err(Positioned::new(
                    &self.get_position(),
                    Error::InvalidString(e),
                ))
            }
        }
    }
    fn parse_number(&mut self) -> Result {
        match number::parse_number(&self.bytes[self.ix..]) {
            Ok((number, consumed_bytes)) => {
                self.ix += consumed_bytes;
                let consumed_chars = consumed_bytes;
                let column_start = self.column;
                let column_end = column_start + consumed_chars - 1;
                self.seek_column_by(consumed_chars);
                Ok(Spanning::start_end(
                    SourcePosition::new(self.line, column_start),
                    SourcePosition::new(self.line, column_end),
                    Token::Number(number),
                ))
            }
            Err(e) => {
                // TODO: need the accurate position from parse_number
                Err(Positioned::new(
                    &self.get_position(),
                    Error::InvalidNumber(e),
                ))
            }
        }
    }
    fn scan_whitespace(&mut self) {
        while self.ix < self.bytes.len() {
            match self.bytes[self.ix] {
                b' ' | b'\t' | b',' => self.seek_column(),
                b'\n' => self.seek_line(),
                b'\r' => {
                    // line is incremented only if the next character is not '\n'
                    if let Some(&b'\n') = self.bytes.get(self.ix + 1) {
                    } else {
                        self.seek_line();
                    }
                }
                _ => break,
            }
            self.ix += 1;
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};
    use crate::{
        ast::spanning::{Positioned, SourcePosition, Spanning},
        mk_name,
    };
    #[cfg(test)]
    use pretty_assertions::assert_eq;

    fn spanned_token(
        start_line: usize,
        start_column: usize,
        end_line: usize,
        end_column: usize,
        token: Token,
    ) -> Spanning<Token> {
        Spanning::start_end(
            SourcePosition::new(start_line, start_column),
            SourcePosition::new(end_line, end_column),
            token,
        )
    }

    fn positioned<I>(start_line: usize, start_column: usize, item: I) -> Positioned<I> {
        Positioned::new(&SourcePosition::new(start_line, start_column), item)
    }

    #[test]
    fn tracks_line_breaks() {
        let foo_name = mk_name!("foo");
        assert_eq!(
            Lexer::new("foo").read_next_token(),
            Some(Ok(spanned_token(1, 1, 1, 3, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new("\nfoo").read_next_token(),
            Some(Ok(spanned_token(2, 1, 2, 3, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new("\rfoo").read_next_token(),
            Some(Ok(spanned_token(2, 1, 2, 3, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new("\r\nfoo").read_next_token(),
            Some(Ok(spanned_token(2, 1, 2, 3, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new("\n\rfoo").read_next_token(),
            Some(Ok(spanned_token(3, 1, 3, 3, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new("\r\r\n\nfoo").read_next_token(),
            Some(Ok(spanned_token(4, 1, 4, 3, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new("\n\n\r\rfoo").read_next_token(),
            Some(Ok(spanned_token(5, 1, 5, 3, Token::from(foo_name))))
        );
    }

    #[test]
    fn records_line_and_column() {
        let foo_name = mk_name!("foo");
        assert_eq!(
            Lexer::new("\n \r\n \r  foo\n").read_next_token(),
            Some(Ok(spanned_token(4, 3, 4, 5, Token::from(foo_name))))
        );
    }

    #[test]
    fn skips_whitespace_and_comments() {
        let foo_name = mk_name!("foo");
        assert_eq!(
            Lexer::new(
                r"

            foo

            "
            )
            .read_next_token(),
            Some(Ok(spanned_token(
                3,
                13,
                3,
                15,
                Token::from(foo_name.clone())
            )))
        );
        assert_eq!(
            Lexer::new(
                r"
            #comment
            foo#comment
            "
            )
            .read_next_token(),
            Some(Ok(spanned_token(
                3,
                13,
                3,
                15,
                Token::from(foo_name.clone())
            )))
        );
        assert_eq!(
            Lexer::new("\t\tfoo\t\t").read_next_token(),
            Some(Ok(spanned_token(1, 3, 1, 5, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new(",,,foo,,,").read_next_token(),
            Some(Ok(spanned_token(1, 4, 1, 6, Token::from(foo_name.clone()))))
        );
        assert_eq!(
            Lexer::new(",,,foo,,,").read_next_token(),
            Some(Ok(spanned_token(1, 4, 1, 6, Token::from(foo_name))))
        );
    }

    #[test]
    fn errors_respect_whitespace() {
        assert_eq!(
            Lexer::new("\n\n ~\n").read_next_token(),
            Some(Err(positioned(
                3,
                2,
                super::Error::UnexpectedCharacter('~')
            )))
        );
    }

    #[test]
    fn lexes_strings() {
        // ""
        assert_eq!(
            Lexer::new(r#""""#).read_next_token(),
            Some(Ok(spanned_token(1, 1, 1, 2, Token::from(""))))
        );
        // "simple"
        assert_eq!(
            Lexer::new(r#""simple""#).read_next_token(),
            Some(Ok(spanned_token(1, 1, 1, 8, Token::from("simple"))))
        );
        // " white space "
        assert_eq!(
            Lexer::new(r#"" white space ""#).read_next_token(),
            Some(Ok(spanned_token(1, 1, 1, 15, Token::from(" white space "))))
        );
        // "quote \""
        assert_eq!(
            Lexer::new(r#""quote \"""#).read_next_token(),
            Some(Ok(spanned_token(1, 1, 1, 10, Token::from("quote \""))))
        );
        // "escaped \n\r\b\t\f"
        assert_eq!(
            Lexer::new(r#""escaped \n\r\b\t\f""#).read_next_token(),
            Some(Ok(spanned_token(
                1,
                1,
                1,
                20,
                Token::from("escaped \n\r\u{8}\t\u{c}")
            )))
        );
        // "escaped \\ \/" -> escaped \ /
        assert_eq!(
            Lexer::new(r#""slashes \\ \/""#).read_next_token(),
            Some(Ok(spanned_token(1, 1, 1, 15, Token::from("slashes \\ /"))))
        );
    }

    #[test]
    fn lexes_block_strings() {
        // """"""
        assert_eq!(
            Lexer::new("\"\"\"\"\"\"").read_next_token(),
            Some(Ok(spanned_token(1, 1, 1, 6, Token::from(""))))
        );
        // """
        //
        // """
        // Two whitespaces on the empty line
        // Leading/trailing empty lines should be removed
        assert_eq!(
            Lexer::new("\"\"\"\n  \n\"\"\"").read_next_token(),
            Some(Ok(spanned_token(1, 1, 3, 3, Token::from(""))))
        );

        // """ first
        //    abc
        //   def
        // """
        // Space indents
        // First line shouldn't count for indent
        // \r\n should be treated as a single line separator
        // Trailing empty lines should be removed
        assert_eq!(
            Lexer::new("\"\"\" first\r\n   abc\r\n  def\r\n\"\"\"").read_next_token(),
            Some(Ok(spanned_token(
                1,
                1,
                4,
                3,
                Token::from(" first\n abc\ndef")
            )))
        );

        // """
        //
        //   abc
        //
        //   def """
        // Tab indents
        // \n\r should be treated as two line separators
        // Empty lines should not count for indents
        // Leading empty lines should be removed
        assert_eq!(
            Lexer::new("\"\"\"\n\r\tabc\n\r\tdef \"\"\"").read_next_token(),
            Some(Ok(spanned_token(1, 1, 5, 8, Token::from("abc\n\ndef "))))
        );

        // """
        //   abc\"""\""""
        //   def\" """
        // Mixed tab and space indents, both should count as one character only
        // Escaping should apply only to \""" and have the appropriate semantics
        assert_eq!(
            Lexer::new("\"\"\"\n\tabc\\\"\"\"\\\"\"\"\"\n  def\\\" \"\"\"").read_next_token(),
            Some(Ok(spanned_token(
                1,
                1,
                3,
                11,
                Token::from("abc\"\"\"\"\"\"\"\n def\\\" ")
            )))
        );
        // """
        //  abc
        //  def
        assert_eq!(
            Lexer::new("\"\"\"\r\tabc\r\tdef").read_next_token(),
            Some(Err(positioned(
                3,
                5,
                super::Error::InvalidString(super::string::Error::Unterminated)
            )))
        );
    }
}
