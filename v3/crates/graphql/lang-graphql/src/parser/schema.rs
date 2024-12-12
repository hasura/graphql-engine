use super::Parser;
use crate::{
    ast::{common::Name, schema::*, spanning::*},
    lexer,
};

fn get_start_position<T, U>(
    first_token: Option<&Spanning<T>>,
    second_token: &Spanning<U>,
) -> SourcePosition {
    if let Some(first_token) = first_token {
        first_token.start
    } else {
        second_token.start
    }
}

fn get_end_position<T, U>(
    preceding_token: &Spanning<T>,
    repeated_tokens: &[Spanning<U>],
) -> SourcePosition {
    if let Some(last_token) = repeated_tokens.last() {
        last_token.end
    } else {
        preceding_token.end
    }
}

impl Parser<'_> {
    /// @ Name Arguments
    fn parse_const_directive(&mut self) -> super::Result<Spanning<ConstDirective>> {
        let start_position = self.parse_punctuation(lexer::Punctuation::At)?.start;
        let name = self.parse_name()?;
        let arguments = self.parse_const_arguments()?;
        let end_position = arguments.as_ref().map_or(&name.end, |l| &l.end);
        Ok(Spanning::start_end(
            start_position,
            *end_position,
            ConstDirective { name, arguments },
        ))
    }

    fn parse_const_directives(&mut self) -> super::Result<Vec<Spanning<ConstDirective>>> {
        self.parse_list(
            |s| s.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::At)),
            Parser::parse_const_directive,
        )
    }

    fn parse_schema_definition(
        &mut self,
        description: Option<Spanning<String>>,
    ) -> super::Result<Spanning<SchemaDefinition>> {
        let start_position = get_start_position(
            description.as_ref(),
            &self.parse_keyword(&super::Keyword::Schema)?,
        );
        let directives = self.parse_const_directives()?;
        let operation_types = self.parse_nonempty_delimited_list(
            lexer::Punctuation::BraceL,
            lexer::Punctuation::BraceR,
            |s| {
                let operation_type = s.parse_operation_type()?;
                s.parse_punctuation(lexer::Punctuation::Colon)?;
                let type_name = s.parse_name()?;
                Ok(Spanning::start_end(
                    operation_type.start,
                    type_name.end,
                    (operation_type, type_name),
                ))
            },
        )?;
        Ok(Spanning::start_end(
            start_position,
            operation_types.end,
            SchemaDefinition {
                description,
                extend: false,
                directives,
                operation_types: operation_types.item,
            },
        ))
    }

    fn parse_scalar_definition(
        &mut self,
        description: Option<Spanning<String>>,
    ) -> super::Result<Spanning<ScalarTypeDefinition>> {
        let start_position = get_start_position(
            description.as_ref(),
            &self.parse_keyword(&super::Keyword::Scalar)?,
        );
        let name = self.parse_name()?;
        let directives = self.parse_const_directives()?;
        Ok(Spanning::start_end(
            start_position,
            get_end_position(&name, &directives),
            ScalarTypeDefinition {
                extend: false,
                description,
                name,
                directives,
            },
        ))
    }

    fn parse_field_arguments(
        &mut self,
    ) -> super::Result<Option<Spanning<Vec<Spanning<InputValueDefinition>>>>> {
        self.parse_optional_nonempty_delimited_list(
            lexer::Punctuation::ParenL,
            lexer::Punctuation::ParenR,
            Parser::parse_input_value_definition,
        )
    }

    fn parse_const_arguments(&mut self) -> super::Result<Option<Spanning<Vec<ConstArgument>>>> {
        self.parse_optional_nonempty_delimited_list(
            lexer::Punctuation::ParenL,
            lexer::Punctuation::ParenR,
            |s| s.parse_key_value(Parser::parse_const_value),
        )
    }

    fn parse_input_value_definition(
        &mut self,
    ) -> super::Result<Spanning<crate::ast::schema::InputValueDefinition>> {
        let description = self.parse_optional_string()?;
        let name = self.parse_name()?;
        self.parse_punctuation(lexer::Punctuation::Colon)?;
        let field_type = self.parse_type()?;
        let default_value =
            if self.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::Equals)) {
                self.parse_punctuation(lexer::Punctuation::Equals)?;
                Some(self.parse_const_value()?)
            } else {
                None
            };
        let end_position = default_value.as_ref().map_or(&field_type.end, |l| &l.end);
        let directives = self.parse_const_directives()?;

        Ok(Spanning::start_end(
            get_start_position(description.as_ref(), &name),
            *end_position,
            InputValueDefinition {
                description,
                name,
                ty: field_type,
                default_value,
                directives,
            },
        ))
    }

    fn parse_field_definition(&mut self) -> super::Result<Spanning<FieldDefinition>> {
        let description = self.parse_optional_string()?;
        let name = self.parse_name()?;
        let arguments = self.parse_field_arguments()?;
        self.parse_punctuation(lexer::Punctuation::Colon)?;
        let field_type = self.parse_type()?;
        let directives = self.parse_const_directives()?;
        Ok(Spanning::start_end(
            get_start_position(description.as_ref(), &name),
            get_end_position(&field_type, &directives),
            FieldDefinition {
                description,
                name,
                ty: field_type,
                arguments: arguments.map_or(Vec::new(), |v| v.item),
                directives,
            },
        ))
    }

    fn parse_fields_definition(
        &mut self,
    ) -> super::Result<Spanning<Vec<Spanning<FieldDefinition>>>> {
        self.parse_nonempty_delimited_list(
            lexer::Punctuation::BraceL,
            lexer::Punctuation::BraceR,
            Parser::parse_field_definition,
        )
    }

    fn parse_implements(&mut self) -> super::Result<Vec<Spanning<Name>>> {
        fn parse_implements_elements(parser: &mut Parser) -> super::Result<Vec<Spanning<Name>>> {
            parser.parse_list(
                |s| s.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::Amp)),
                |s| {
                    s.parse_punctuation(lexer::Punctuation::Amp)?;
                    s.parse_name()
                },
            )
        }
        if self
            .parse_optional(
                |token| matches!(token, lexer::Token::Name(name) if name.get() == super::Keyword::Implements.as_str()),
                |parser| parser.parse_keyword(&super::Keyword::Implements)
            )?
            .is_some()
        {
            static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
                super::ExpectedToken::Punctuation(lexer::Punctuation::Amp),
                super::ExpectedToken::Name,
            ];
            match self.peek() {
                None => self.eof(EXPECTED_TOKENS),
                Some(Err(e)) => Self::lexer_error(e, EXPECTED_TOKENS),
                Some(Ok(token)) => match &token.item {
                    lexer::Token::Punctuation(lexer::Punctuation::Amp) => {
                        parse_implements_elements(self)
                    }
                    lexer::Token::Name(_name) => {
                        let mut members = Vec::from([self.parse_name()?]);
                        members.extend(parse_implements_elements(self)?);
                        Ok(members)
                    }
                    _ => Err(Positioned::new(
                        &token.start,
                        super::Error::TokenError{
                            expected_tokens: EXPECTED_TOKENS,
                            found: super::TokenFound::Token(token.item.clone()),
                        },
                    )),
                },
            }
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_object_definition(
        &mut self,
        description: Option<Spanning<String>>,
    ) -> super::Result<Spanning<ObjectTypeDefinition>> {
        let start_position = get_start_position(
            description.as_ref(),
            &self.parse_keyword(&super::Keyword::Type)?,
        );
        let name = self.parse_name()?;
        let implements = self.parse_implements()?;
        let directives = self.parse_const_directives()?;
        let fields = self.parse_fields_definition()?;
        Ok(Spanning::start_end(
            start_position,
            fields.end,
            ObjectTypeDefinition {
                description,
                name,
                directives,
                extend: false,
                implements,
                fields: fields.item,
            },
        ))
    }

    fn parse_interface_definition(
        &mut self,
        description: Option<Spanning<String>>,
    ) -> super::Result<Spanning<InterfaceTypeDefinition>> {
        let start_position = get_start_position(
            description.as_ref(),
            &self.parse_keyword(&super::Keyword::Interface)?,
        );
        let name = self.parse_name()?;
        let implements = self.parse_implements()?;
        let directives = self.parse_const_directives()?;
        let fields = self.parse_fields_definition()?;
        Ok(Spanning::start_end(
            start_position,
            fields.end,
            InterfaceTypeDefinition {
                description,
                name,
                directives,
                extend: false,
                implements,
                fields: fields.item,
            },
        ))
    }

    fn parse_union_members(&mut self) -> super::Result<Vec<Spanning<Name>>> {
        fn parse_member_elements(parser: &mut Parser) -> super::Result<Vec<Spanning<Name>>> {
            parser.parse_list(
                |s| s.is_next_token(&lexer::Token::Punctuation(lexer::Punctuation::Pipe)),
                |s| {
                    s.parse_punctuation(lexer::Punctuation::Pipe)?;
                    s.parse_name()
                },
            )
        }
        if self
            .parse_optional(
                |token| token.is_punctuation(lexer::Punctuation::Equals),
                |parser| parser.parse_punctuation(lexer::Punctuation::Equals),
            )?
            .is_some()
        {
            static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
                super::ExpectedToken::Punctuation(lexer::Punctuation::Pipe),
                super::ExpectedToken::Name,
            ];
            match self.peek() {
                None => self.eof(EXPECTED_TOKENS),
                Some(Err(e)) => Self::lexer_error(e, EXPECTED_TOKENS),
                Some(Ok(token)) => match &token.item {
                    lexer::Token::Punctuation(lexer::Punctuation::Pipe) => {
                        parse_member_elements(self)
                    }
                    lexer::Token::Name(_name) => {
                        let mut members = Vec::from([self.parse_name()?]);
                        members.extend(parse_member_elements(self)?);
                        Ok(members)
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
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_union_definition(
        &mut self,
        description: Option<Spanning<String>>,
    ) -> super::Result<Spanning<UnionTypeDefinition>> {
        let start_position = get_start_position(
            description.as_ref(),
            &self.parse_keyword(&super::Keyword::Union)?,
        );
        let name = self.parse_name()?;
        let directives = self.parse_const_directives()?;
        let members = self.parse_union_members()?;
        let end_position = members
            .last()
            .map_or(get_end_position(&name, &directives), |member| member.end);
        Ok(Spanning::start_end(
            start_position,
            end_position,
            UnionTypeDefinition {
                description,
                name,
                directives,
                extend: false,
                members,
            },
        ))
    }

    fn parse_enum_definition(
        &mut self,
        description: Option<Spanning<String>>,
    ) -> super::Result<Spanning<EnumTypeDefinition>> {
        let start_position = get_start_position(
            description.as_ref(),
            &self.parse_keyword(&super::Keyword::Enum)?,
        );
        let name = self.parse_name()?;
        let directives = self.parse_const_directives()?;
        let values = self.parse_nonempty_delimited_list(
            lexer::Punctuation::BraceL,
            lexer::Punctuation::BraceR,
            |s| {
                let description = s.parse_optional_string()?;
                let value = s.parse_name()?;
                let directives = s.parse_const_directives()?;
                Ok(Spanning::start_end(
                    get_start_position(description.as_ref(), &value),
                    get_end_position(&value, &directives),
                    EnumValueDefinition {
                        description,
                        value,
                        directives,
                    },
                ))
            },
        )?;
        Ok(Spanning::start_end(
            start_position,
            values.end,
            EnumTypeDefinition {
                description,
                name,
                directives,
                extend: false,
                values: values.item,
            },
        ))
    }

    fn parse_input_definition(
        &mut self,
        description: Option<Spanning<String>>,
    ) -> super::Result<Spanning<InputObjectTypeDefinition>> {
        let start_position = get_start_position(
            description.as_ref(),
            &self.parse_keyword(&super::Keyword::Input)?,
        );
        let name = self.parse_name()?;
        let directives = self.parse_const_directives()?;
        let fields = self.parse_nonempty_delimited_list(
            lexer::Punctuation::BraceL,
            lexer::Punctuation::BraceR,
            Parser::parse_input_value_definition,
        )?;
        Ok(Spanning::start_end(
            start_position,
            fields.end,
            InputObjectTypeDefinition {
                description,
                name,
                directives,
                extend: false,
                fields: fields.item,
            },
        ))
    }

    fn parse_type_system_definition(&mut self) -> super::Result<Spanning<TypeSystemDefinition>> {
        static EXPECTED_TOKENS: &[super::ExpectedToken] = &[
            super::ExpectedToken::Keyword(super::Keyword::Schema),
            super::ExpectedToken::Keyword(super::Keyword::Scalar),
            super::ExpectedToken::Keyword(super::Keyword::Type),
            super::ExpectedToken::Keyword(super::Keyword::Interface),
            super::ExpectedToken::Keyword(super::Keyword::Union),
            super::ExpectedToken::Keyword(super::Keyword::Enum),
            super::ExpectedToken::Keyword(super::Keyword::Input),
        ];
        let description = self.parse_optional_string()?;
        match self.peek() {
            None => self.eof(EXPECTED_TOKENS),
            Some(Err(e)) => Self::lexer_error(e, EXPECTED_TOKENS),
            Some(Ok(token)) => match &token.item {
                lexer::Token::Name(name) => match name.as_str().parse() {
                    Ok(super::Keyword::Schema) => self
                        .parse_schema_definition(description)
                        .map(|s| s.map(TypeSystemDefinition::Schema)),
                    Ok(super::Keyword::Scalar) => self
                        .parse_scalar_definition(description)
                        .map(|s| s.map(|d| TypeSystemDefinition::Type(TypeDefinition::Scalar(d)))),
                    Ok(super::Keyword::Type) => self
                        .parse_object_definition(description)
                        .map(|s| s.map(|d| TypeSystemDefinition::Type(TypeDefinition::Object(d)))),
                    Ok(super::Keyword::Interface) => {
                        self.parse_interface_definition(description).map(|s| {
                            s.map(|d| TypeSystemDefinition::Type(TypeDefinition::Interface(d)))
                        })
                    }
                    Ok(super::Keyword::Union) => self
                        .parse_union_definition(description)
                        .map(|s| s.map(|d| TypeSystemDefinition::Type(TypeDefinition::Union(d)))),
                    Ok(super::Keyword::Enum) => self
                        .parse_enum_definition(description)
                        .map(|s| s.map(|d| TypeSystemDefinition::Type(TypeDefinition::Enum(d)))),
                    Ok(super::Keyword::Input) => {
                        self.parse_input_definition(description).map(|s| {
                            s.map(|d| TypeSystemDefinition::Type(TypeDefinition::InputObject(d)))
                        })
                    }
                    _ => Err(Positioned::new(
                        &token.start,
                        super::Error::TokenError {
                            expected_tokens: EXPECTED_TOKENS,
                            found: super::TokenFound::Token(token.item.clone()),
                        },
                    )),
                },
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
    pub fn parse_schema_document(&mut self) -> super::Result<SchemaDocument> {
        let mut definitions = vec![];
        while self.peek().is_some() {
            definitions.push(self.parse_type_system_definition()?);
        }
        Ok(SchemaDocument { definitions })
    }
}
