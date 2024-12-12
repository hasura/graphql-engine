use super::Parser;
use crate::{
    ast::{
        common::{BaseType, OperationType, Type, TypeName},
        executable::{
            ExecutableDefinition, ExecutableDocument, OperationDefinition, VariableDefinition,
        },
        spanning::{Positioned, Spanning},
    },
    lexer,
};
use recursion_limit_macro::limit_recursion;

impl Parser<'_> {
    fn parse_base_type(&mut self) -> super::Result<Spanning<BaseType>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Name,
            super::ExpectedToken::Punctuation(lexer::Punctuation::BracketL),
        ];
        match self.peek() {
            None => self.eof(EXPECTED_TOKENS),
            Some(Err(e)) => Self::lexer_error(e, EXPECTED_TOKENS),
            Some(Ok(token)) => match &token.item {
                lexer::Token::Name(_) => {
                    Ok(self.parse_name()?.map(|n| BaseType::Named(TypeName(n))))
                }
                lexer::Token::Punctuation(lexer::Punctuation::BracketL) => {
                    let start_position =
                        self.parse_punctuation(lexer::Punctuation::BracketL)?.start;
                    let wrapped_type = self.parse_type()?;
                    let end_position = self.parse_punctuation(lexer::Punctuation::BracketR)?.start;
                    Ok(Spanning::start_end(
                        start_position,
                        end_position,
                        BaseType::List(Box::new(wrapped_type.item)),
                    ))
                }
                _ => Err(Positioned::new(
                    &token.start,
                    super::Error::TokenError {
                        expected_tokens: EXPECTED_TOKENS,
                        found: super::TokenFound::Token(token.item.clone()),
                    },
                )),
            },
        }
    }
    #[limit_recursion]
    pub fn parse_type(&mut self) -> super::Result<Spanning<Type>> {
        let base = self.parse_base_type()?;
        let bang = if self.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::Bang)) {
            Some(self.parse_punctuation(lexer::Punctuation::Bang)?)
        } else {
            None
        };
        let nullable = bang.is_none();
        Ok(Spanning::start_end(
            base.start,
            bang.map_or(base.end, |p| p.end),
            Type {
                base: base.item,
                nullable,
            },
        ))
    }
    fn parse_variable_definition(&mut self) -> super::Result<Spanning<VariableDefinition>> {
        let start_position = self.parse_punctuation(lexer::Punctuation::Dollar)?.start;
        let name = self.parse_name()?;
        self.parse_punctuation(lexer::Punctuation::Colon)?;
        let var_type = self.parse_type()?;
        let default_value =
            if self.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::Equals)) {
                self.parse_punctuation(lexer::Punctuation::Equals)?;
                Some(self.parse_const_value()?)
            } else {
                None
            };
        let end_position = default_value.as_ref().map_or(&var_type.end, |l| &l.end);

        Ok(Spanning::start_end(
            start_position,
            *end_position,
            VariableDefinition {
                name,
                var_type,
                default_value,
            },
        ))
    }
    pub fn parse_variable_definitions(
        &mut self,
    ) -> super::Result<Option<Spanning<Vec<Spanning<VariableDefinition>>>>> {
        self.parse_optional_nonempty_delimited_list(
            lexer::Punctuation::ParenL,
            lexer::Punctuation::ParenR,
            Parser::parse_variable_definition,
        )
    }

    pub fn parse_operation_type(&mut self) -> super::Result<Spanning<OperationType>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Keyword(super::Keyword::Query),
            super::ExpectedToken::Keyword(super::Keyword::Mutation),
            super::ExpectedToken::Keyword(super::Keyword::Subscription),
        ];

        match self.peek() {
            None => self.eof(EXPECTED_TOKENS),
            Some(Err(e)) => Self::lexer_error(e, EXPECTED_TOKENS),
            Some(Ok(token)) => match &token.item {
                lexer::Token::Name(_) => {
                    let token = self.parse_name()?;
                    if token.item.is_keyword(&super::Keyword::Query) {
                        Ok(Spanning::start_end(
                            token.start,
                            token.end,
                            OperationType::Query,
                        ))
                    } else if token.item.is_keyword(&super::Keyword::Mutation) {
                        Ok(Spanning::start_end(
                            token.start,
                            token.end,
                            OperationType::Mutation,
                        ))
                    } else if token.item.is_keyword(&super::Keyword::Subscription) {
                        Ok(Spanning::start_end(
                            token.start,
                            token.end,
                            OperationType::Subscription,
                        ))
                    } else {
                        Err(Positioned::new(
                            &token.start,
                            super::Error::TokenError {
                                expected_tokens: EXPECTED_TOKENS,
                                found: super::TokenFound::Token(lexer::Token::Name(token.item)),
                            },
                        ))
                    }
                }
                _ => Err(Positioned::new(
                    &token.start,
                    super::Error::TokenError {
                        expected_tokens: EXPECTED_TOKENS,
                        found: super::TokenFound::Token(token.item.clone()),
                    },
                )),
            },
        }
    }

    pub fn parse_operation(&mut self) -> super::Result<Spanning<OperationDefinition>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Keyword(super::Keyword::Query),
            super::ExpectedToken::Keyword(super::Keyword::Mutation),
            super::ExpectedToken::Keyword(super::Keyword::Subscription),
            super::ExpectedToken::Punctuation(lexer::Punctuation::BraceL),
        ];
        let operation_type = match self.peek() {
            None => self.eof(EXPECTED_TOKENS),
            Some(Err(e)) => Self::lexer_error(e, EXPECTED_TOKENS),
            Some(Ok(token)) => match &token.item {
                lexer::Token::Name(_) => self.parse_operation_type().map(Some),
                lexer::Token::Punctuation(lexer::Punctuation::BraceL) => Ok(None),
                _ => Err(Positioned::new(
                    &token.start,
                    super::Error::TokenError {
                        expected_tokens: EXPECTED_TOKENS,
                        found: super::TokenFound::Token(token.item.clone()),
                    },
                )),
            },
        }?;
        if let Some(operation_type) = operation_type {
            let name = if let Some(Ok(token)) = self.peek() {
                if let lexer::Token::Name(_) = &token.item {
                    Some(self.parse_name()?)
                } else {
                    None
                }
            } else {
                None
            };
            let variable_definitions = self.parse_variable_definitions()?;
            let directives = self.parse_directives()?;
            let selection_set = self.parse_selection_set()?;
            Ok(Spanning::start_end(
                operation_type.start,
                selection_set.end,
                OperationDefinition {
                    ty: operation_type.item,
                    name,
                    variable_definitions,
                    directives,
                    selection_set,
                },
            ))
        } else {
            let selection_set = self.parse_selection_set()?;
            Ok(Spanning::start_end(
                selection_set.start,
                selection_set.end,
                OperationDefinition {
                    ty: OperationType::Query,
                    name: None,
                    variable_definitions: None,
                    directives: vec![],
                    selection_set,
                },
            ))
        }
    }
    // https://spec.graphql.org/October2021/#sec-Executable-Definitions
    pub fn parse_executable_definition(&mut self) -> super::Result<Spanning<ExecutableDefinition>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Keyword(super::Keyword::Query),
            super::ExpectedToken::Keyword(super::Keyword::Mutation),
            super::ExpectedToken::Keyword(super::Keyword::Subscription),
            super::ExpectedToken::Keyword(super::Keyword::Fragment),
            super::ExpectedToken::Punctuation(lexer::Punctuation::BraceL),
        ];
        match self.peek() {
            None => self.eof(EXPECTED_TOKENS),
            Some(Err(e)) => Self::lexer_error(e, EXPECTED_TOKENS),
            Some(Ok(token)) => match &token.item {
                lexer::Token::Name(name) => {
                    if name.is_keyword(&super::Keyword::Fragment) {
                        Ok(self.parse_fragment()?.map(ExecutableDefinition::Fragment))
                    } else {
                        Ok(self.parse_operation()?.map(ExecutableDefinition::Operation))
                    }
                }
                lexer::Token::Punctuation(lexer::Punctuation::BraceL) => {
                    Ok(self.parse_operation()?.map(ExecutableDefinition::Operation))
                }
                _ => Err(Positioned::new(
                    &token.start,
                    super::Error::TokenError {
                        expected_tokens: EXPECTED_TOKENS,
                        found: super::TokenFound::Token(token.item.clone()),
                    },
                )),
            },
        }
    }
    /// The top-level parser for a graphql request
    pub fn parse_executable_document(&mut self) -> super::Result<ExecutableDocument> {
        let mut items = vec![];
        while self.peek().is_some() {
            items.push(self.parse_executable_definition()?);
        }
        Ok(ExecutableDocument { items })
    }
}
