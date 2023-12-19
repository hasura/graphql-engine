use lang_graphql::ast::common::Alias;
use open_dds::types::FieldName;

const GLOBAL_ID_NDC_PREFIX: &str = "hasura_global_id_col";
pub const GLOBAL_ID_VERSION: u16 = 1;

pub fn global_id_col_format(alias: &Alias, field_name: &FieldName) -> String {
    format!(
        "{}_{}_{}",
        GLOBAL_ID_NDC_PREFIX.to_owned(),
        alias,
        &field_name.0
    )
}
