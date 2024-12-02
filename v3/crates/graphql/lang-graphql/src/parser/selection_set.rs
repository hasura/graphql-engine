use super::Parser;
use crate::{
    ast::{
        common::{Alias, TypeName},
        executable::{
            Argument, Directive, Field, FragmentSpread, InlineFragment, Selection, SelectionSet,
            TypeCondition,
        },
        spanning::Spanning,
    },
    lexer,
};
use recursion_limit_macro::limit_recursion;

impl<'a> Parser<'a> {
    pub fn parse_type_condition(&mut self) -> super::Result<Spanning<TypeCondition>> {
        let on = self.parse_keyword(&super::Keyword::On)?;
        let type_name = self.parse_name()?;
        Ok(Spanning::start_end(
            on.start,
            type_name.end,
            TypeCondition {
                on: type_name.map(TypeName),
            },
        ))
    }

    fn parse_selection(&mut self) -> super::Result<Spanning<Selection>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Name,
            super::ExpectedToken::Punctuation(lexer::Punctuation::Spread),
        ];
        let token = self.peek_fail(EXPECTED_TOKENS)?;
        match &token.item {
            lexer::Token::Name(_) => Ok(self.parse_field()?.map(Selection::Field)),
            lexer::Token::Punctuation(lexer::Punctuation::Spread) => self.parse_spread(),
            _ => Self::unexpected_token(EXPECTED_TOKENS, token.item.clone(), &token.start),
        }
    }

    #[limit_recursion]
    pub fn parse_selection_set(&mut self) -> super::Result<Spanning<SelectionSet>> {
        self.parse_nonempty_delimited_list(
            lexer::Punctuation::BraceL,
            lexer::Punctuation::BraceR,
            Parser::parse_selection,
        )
        .map(|r| r.map(|l| SelectionSet { items: l }))
    }

    // These 'inline' attributes contribute to about 10% improvement in parse times
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn parse_field(&mut self) -> super::Result<Spanning<Field>> {
        let name1 = self.parse_name()?;
        let name2 = if self.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::Colon)) {
            self.parse_punctuation(lexer::Punctuation::Colon)?;
            Some(self.parse_name()?)
        } else {
            None
        };
        let (alias, name) = match name2 {
            Some(name) => (Some(name1.map(Alias::new)), name),
            None => (None, name1),
        };
        let arguments = self.parse_arguments()?;
        let directives = self.parse_directives()?;
        let selection_set =
            if self.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::BraceL)) {
                Some(self.parse_selection_set()?)
            } else {
                None
            };
        Ok(Spanning::start_end(
            // TODO
            name.start,
            name.end,
            Field {
                alias,
                name,
                arguments,
                directives,
                selection_set,
            },
        ))
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn parse_spread(&mut self) -> super::Result<Spanning<Selection>> {
        let spread = self.parse_punctuation(lexer::Punctuation::Spread)?;
        let is_inline_spread = self.is_next_token_keyword(&super::Keyword::On)
            || self.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::At))
            || self.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::BraceL));
        if is_inline_spread {
            let type_condition = if self.is_next_token_keyword(&super::Keyword::On) {
                Some(self.parse_type_condition()?)
            } else {
                None
            };

            let directives = self.parse_directives()?;
            let selection_set = self.parse_selection_set()?;
            Ok(Spanning::start_end(
                spread.start,
                selection_set.end,
                Selection::InlineFragment(InlineFragment {
                    type_condition,
                    directives,
                    selection_set,
                }),
            ))
        } else {
            let fragment_name = self.parse_name()?;
            let directives = self.parse_directives()?;
            Ok(Spanning::start_end(
                spread.start,
                // TODO: Include the last directive's end position here?
                fragment_name.end,
                Selection::FragmentSpread(FragmentSpread {
                    fragment_name,
                    directives,
                }),
            ))
        }
    }

    pub fn parse_arguments(&mut self) -> super::Result<Option<Spanning<Vec<Argument>>>> {
        self.parse_optional_nonempty_delimited_list(
            lexer::Punctuation::ParenL,
            lexer::Punctuation::ParenR,
            |s| s.parse_key_value(Parser::parse_value),
        )
    }

    /// @ Name Arguments
    fn parse_directive(&mut self) -> super::Result<Spanning<Directive>> {
        let start_position = self.parse_punctuation(lexer::Punctuation::At)?.start;
        let name = self.parse_name()?;
        let arguments = self.parse_arguments()?;
        let end_position = arguments.as_ref().map_or(&name.end, |l| &l.end);
        Ok(Spanning::start_end(
            start_position,
            *end_position,
            Directive { name, arguments },
        ))
    }

    pub fn parse_directives(&mut self) -> super::Result<Vec<Spanning<Directive>>> {
        self.parse_list(
            |s| s.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::At)),
            Parser::parse_directive,
        )
    }
}
