use lang_graphql::{ast::common as ast, schema as gql_schema};
use open_dds::arguments::ArgumentName;

use crate::{types, Annotation, GDS};

pub fn generate_field_argument(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    argument_name: &ArgumentName,
    argument_type: &metadata_resolve::FieldArgumentInfo,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>,
    ),
    crate::Error,
> {
    let field_name = ast::Name::new(argument_name.as_str())?;
    let input_type = types::input_type::get_input_type(gds, builder, &argument_type.argument_type)?;

    let input_field = gql_schema::InputField::new(
        field_name.clone(),
        argument_type.description.clone(),
        Annotation::Input(types::InputAnnotation::FieldArgument {
            argument_name: argument_name.clone(),
        }),
        input_type,
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    );

    Ok((field_name, builder.allow_all_namespaced(input_field)))
}
