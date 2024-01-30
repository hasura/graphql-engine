use super::Parser;
use crate::ast::{executable::FragmentDefinition, spanning::Spanning};

impl<'a> Parser<'a> {
    // https://spec.graphql.org/October2021/#sec-Language.Fragments
    pub fn parse_fragment(&mut self) -> super::Result<Spanning<FragmentDefinition>> {
        let start_position = self.parse_keyword(&super::Keyword::Fragment)?.start;
        let name = self.parse_name()?;
        // It seems like this is in the spec to make parsing easier, or possibly unambiguous in the
        // presence of directives?
        if name.item.as_str() == "on" {
            return Self::other_error(
                "A fragment may not be named \"on\". Maybe you forgot the name?",
                name.start,
            );
        }
        let type_condition = self.parse_type_condition()?;
        let directives = self.parse_directives()?;
        let selection_set = self.parse_selection_set()?;
        Ok(Spanning::start_end(
            start_position,
            selection_set.end,
            FragmentDefinition {
                name,
                type_condition,
                directives,
                selection_set,
            },
        ))
    }
}
