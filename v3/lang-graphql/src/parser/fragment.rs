use super::Parser;
use crate::ast::{executable::FragmentDefinition, spanning::Spanning};

impl<'a> Parser<'a> {
    pub fn parse_fragment(&mut self) -> super::Result<Spanning<FragmentDefinition>> {
        let start_position = self.parse_keyword(&super::Keyword::Fragment)?.start;
        let name = self.parse_name()?;
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
