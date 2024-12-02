use crate::{
    ast::{
        spanning::Spanning,
        value::KeyValue,
        value::SimpleValue,
        value::{ConstValue, Value},
    },
    lexer,
};
use recursion_limit_macro::limit_recursion;

use super::Parser;

impl<'a> Parser<'a> {
    fn parse_number(&mut self) -> super::Result<Spanning<lexer::number::NumberToken>> {
        static EXPECTED: &[super::ExpectedToken] = &[super::ExpectedToken::Number];
        self.parse_token(EXPECTED, |token| {
            if let lexer::Token::Number(number) = token.item {
                Ok(Spanning::start_end(token.start, token.end, number))
            } else {
                Self::unexpected_token(EXPECTED, token.item, &token.start)
            }
        })
    }

    fn parse_string(&mut self) -> super::Result<Spanning<String>> {
        static EXPECTED: &[super::ExpectedToken] = &[super::ExpectedToken::String];
        self.parse_token(EXPECTED, |token| {
            if let lexer::Token::String(string) = token.item {
                Ok(Spanning::start_end(token.start, token.end, string))
            } else {
                Self::unexpected_token(EXPECTED, token.item, &token.start)
            }
        })
    }

    pub fn parse_optional_string(&mut self) -> super::Result<Option<Spanning<String>>> {
        self.parse_optional(
            |token| matches!(token, lexer::Token::String(_)),
            Parser::parse_string,
        )
    }

    fn parse_simple_value(
        &mut self,
        expected_tokens: &'static [super::ExpectedToken],
    ) -> super::Result<Spanning<SimpleValue>> {
        let token = self.peek_fail(expected_tokens)?;
        match &token.item {
            lexer::Token::Name(_) => {
                let token = self.parse_name()?;
                let name = token.item.get();
                let value = if name == "false" {
                    SimpleValue::Boolean(false)
                } else if name == "true" {
                    SimpleValue::Boolean(true)
                } else if name == "null" {
                    SimpleValue::Null
                } else {
                    SimpleValue::Enum(token.item)
                };
                Ok(Spanning::start_end(token.start, token.end, value))
            }
            lexer::Token::Number(_) => {
                let token = self.parse_number()?;
                Ok(token.map(|number| match number {
                    lexer::number::NumberToken::Float64(f64) => SimpleValue::Float(f64),
                    lexer::number::NumberToken::Int64(i64) => SimpleValue::Integer(i64),
                }))
            }
            lexer::Token::String(_) => {
                let token = self.parse_string()?;
                Ok(token.map(SimpleValue::String))
            }
            lexer::Token::Punctuation(_) => {
                Self::unexpected_token(expected_tokens, token.item.clone(), &token.start)
            }
        }
    }

    pub fn parse_key_value<T, F>(&mut self, parse: F) -> super::Result<Spanning<KeyValue<T>>>
    where
        F: Fn(&mut Self) -> super::Result<Spanning<T>>,
    {
        let key = self.parse_name()?;
        self.parse_punctuation(lexer::Punctuation::Colon)?;
        let value = parse(self)?;
        Ok(Spanning::start_end(
            key.start,
            value.end,
            KeyValue { key, value },
        ))
    }

    #[limit_recursion]
    pub fn parse_const_value(&mut self) -> super::Result<Spanning<ConstValue>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Number,
            super::ExpectedToken::String,
            super::ExpectedToken::Keyword(super::Keyword::True),
            super::ExpectedToken::Keyword(super::Keyword::False),
            super::ExpectedToken::Keyword(super::Keyword::Null),
            super::ExpectedToken::Punctuation(lexer::Punctuation::BracketL),
            super::ExpectedToken::Punctuation(lexer::Punctuation::BraceL),
        ];
        match self.peek_fail(EXPECTED_TOKENS)?.item {
            lexer::Token::Punctuation(lexer::Punctuation::BracketL) => {
                let list = self.parse_possibly_empty_delimited_list(
                    lexer::Punctuation::BracketL,
                    lexer::Punctuation::BracketR,
                    Parser::parse_const_value,
                )?;
                Ok(list.map(ConstValue::List))
            }
            lexer::Token::Punctuation(lexer::Punctuation::BraceL) => {
                let list = self.parse_possibly_empty_delimited_list(
                    lexer::Punctuation::BraceL,
                    lexer::Punctuation::BraceR,
                    |s| s.parse_key_value(Parser::parse_const_value),
                )?;
                Ok(list.map(ConstValue::Object))
            }
            _ => self
                .parse_simple_value(EXPECTED_TOKENS)
                .map(|r| r.map(ConstValue::SimpleValue)),
        }
    }

    #[limit_recursion]
    pub fn parse_value(&mut self) -> super::Result<Spanning<Value>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Number,
            super::ExpectedToken::String,
            super::ExpectedToken::Keyword(super::Keyword::True),
            super::ExpectedToken::Keyword(super::Keyword::False),
            super::ExpectedToken::Keyword(super::Keyword::Null),
            super::ExpectedToken::Punctuation(lexer::Punctuation::Dollar),
            super::ExpectedToken::Punctuation(lexer::Punctuation::BracketL),
            super::ExpectedToken::Punctuation(lexer::Punctuation::BraceL),
        ];
        match self.peek_fail(EXPECTED_TOKENS)?.item {
            // $variable
            lexer::Token::Punctuation(lexer::Punctuation::Dollar) => {
                let punctuation = self.parse_punctuation(lexer::Punctuation::Dollar)?;
                let variable_name = self.parse_name()?;
                Ok(Spanning::start_end(
                    punctuation.start,
                    variable_name.end,
                    Value::Variable(variable_name.item),
                ))
            }
            // [ (<value>,)* ]
            lexer::Token::Punctuation(lexer::Punctuation::BracketL) => {
                let list = self.parse_possibly_empty_delimited_list(
                    lexer::Punctuation::BracketL,
                    lexer::Punctuation::BracketR,
                    Parser::parse_value,
                )?;
                Ok(list.map(Value::List))
            }
            // { (<key>: <value>,)* }
            lexer::Token::Punctuation(lexer::Punctuation::BraceL) => {
                let list = self.parse_possibly_empty_delimited_list(
                    lexer::Punctuation::BraceL,
                    lexer::Punctuation::BraceR,
                    |s| s.parse_key_value(Parser::parse_value),
                )?;
                Ok(list.map(Value::Object))
            }
            // true, false, null, <number>, <string>, <name>
            _ => self
                .parse_simple_value(EXPECTED_TOKENS)
                .map(|r| r.map(Value::SimpleValue)),
        }
    }
}
