use crate::metadata::resolved::error::{Error, TypeMappingValidationError};
use crate::metadata::resolved::ndc_validation::get_underlying_named_type;
use crate::metadata::resolved::relationship::Relationship;
use crate::metadata::resolved::subgraph::{
    mk_qualified_type_reference, Qualified, QualifiedBaseType, QualifiedTypeName,
    QualifiedTypeReference,
};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use ndc_client as ndc;
use open_dds::commands;
use open_dds::permissions::{Role, TypeOutputPermission, TypePermissionsV1};
use open_dds::types::{self, CustomTypeName, FieldName, ObjectTypeV1, TypeName};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::str::FromStr;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
pub enum TypeRepresentation {
    Object(ObjectTypeRepresentation),
    #[display(fmt = "ScalarType")]
    ScalarType {
        graphql_type_name: Option<ast::TypeName>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ObjectTypeRepresentation {
    pub fields: IndexMap<FieldName, FieldDefinition>,
    pub relationships: IndexMap<ast::Name, Relationship>,
    pub type_permissions: HashMap<Role, TypeOutputPermission>,
    pub global_id_fields: Vec<FieldName>,
    pub graphql_output_type_name: Option<ast::TypeName>,
    pub graphql_input_type_name: Option<ast::TypeName>,
    // TODO: add graphql_output_type_kind if we support creating interfaces.
}

pub struct ScalarTypeInfo<'a> {
    pub scalar_type: &'a ndc::models::ScalarType,
    pub representation: Option<TypeName>,
    pub comparison_expression_name: Option<ast::TypeName>,
}

impl<'a> ScalarTypeInfo<'a> {
    pub(crate) fn new(source_scalar: &'a ndc::models::ScalarType) -> Self {
        ScalarTypeInfo {
            scalar_type: source_scalar,
            representation: None,
            comparison_expression_name: None,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FieldDefinition {
    pub field_type: QualifiedTypeReference,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldMapping {
    pub column: String,
    pub column_type: ndc::models::Type,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum TypeMapping {
    Object {
        field_mappings: BTreeMap<FieldName, FieldMapping>,
    },
}

pub fn check_conflicting_graphql_types(
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    new_graphql_type: Option<&ast::TypeName>,
) -> Result<(), Error> {
    if let Some(new_graphql_type) = new_graphql_type {
        // Fail on conflicting graphql type names
        if !(existing_graphql_types.insert(new_graphql_type.clone())) {
            return Err(Error::ConflictingGraphQlType {
                graphql_type_name: new_graphql_type.clone(),
            });
        }
    }
    Ok(())
}

pub fn resolve_field(
    field: &types::FieldDefinition,
    subgraph: &str,
) -> Result<FieldDefinition, Error> {
    Ok(FieldDefinition {
        field_type: mk_qualified_type_reference(&field.field_type, subgraph),
    })
}

pub fn resolve_object_type(
    object_type_definition: &ObjectTypeV1,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    qualified_type_name: &Qualified<CustomTypeName>,
    subgraph: &str,
    global_id_enabled_types: &mut HashMap<
        Qualified<CustomTypeName>,
        Vec<Qualified<open_dds::models::ModelName>>,
    >,
) -> Result<TypeRepresentation, Error> {
    let mut resolved_fields = IndexMap::new();
    let mut resolved_global_id_fields = Vec::new();

    for field in &object_type_definition.fields {
        if resolved_fields
            .insert(field.name.clone(), resolve_field(field, subgraph)?)
            .is_some()
        {
            return Err(Error::DuplicateFieldDefinition {
                type_name: qualified_type_name.clone(),
                field_name: field.name.clone(),
            });
        }
    }
    match &object_type_definition.global_id_fields {
        Some(global_id_fields) => {
            if !global_id_fields.is_empty() {
                // Throw error if the object type has a field called id" and has global fields configured.
                // Because, when the global id fields are configured, the `id` field will be auto-generated.
                if resolved_fields.contains_key(&FieldName("id".into())) {
                    return Err(Error::IdFieldConflictingGlobalId {
                        type_name: qualified_type_name.clone(),
                    });
                }
                // To check if global_id_fields are defined in object type but no model has global_id_source set to
                // true:
                //   - If the object type has globalIdFields configured, add the object type to the
                //     global_id_enabled_types map.
                global_id_enabled_types.insert(qualified_type_name.clone(), Vec::new());
            };
            for global_id_field in global_id_fields {
                if !resolved_fields.contains_key(global_id_field) {
                    return Err(Error::UnknownFieldInGlobalId {
                        field_name: global_id_field.clone(),
                        type_name: qualified_type_name.clone(),
                    });
                } else {
                    resolved_global_id_fields.push(global_id_field.clone())
                }
            }
        }
        None => {}
    }
    let (graphql_type_name, graphql_input_type_name) = match object_type_definition.graphql.as_ref()
    {
        None => Ok::<_, Error>((None, None)),
        Some(graphql) => {
            let graphql_type_name = graphql
                .type_name
                .as_ref()
                .map(|type_name| mk_name(type_name.0.as_ref()).map(ast::TypeName))
                .transpose()?;
            let graphql_input_type_name = graphql
                .input_type_name
                .as_ref()
                .map(|input_type_name| mk_name(input_type_name.0.as_ref()).map(ast::TypeName))
                .transpose()?;
            Ok((graphql_type_name, graphql_input_type_name))
        }
    }?;
    check_conflicting_graphql_types(existing_graphql_types, graphql_type_name.as_ref())?;
    check_conflicting_graphql_types(existing_graphql_types, graphql_input_type_name.as_ref())?;
    Ok(TypeRepresentation::Object(ObjectTypeRepresentation {
        fields: resolved_fields,
        relationships: IndexMap::new(),
        global_id_fields: resolved_global_id_fields,
        type_permissions: HashMap::new(),
        graphql_output_type_name: graphql_type_name,
        graphql_input_type_name,
    }))
}

pub fn get_column<'a>(
    ndc_type: &'a ndc::models::ObjectType,
    field_name: &FieldName,
    column: &str,
) -> Result<&'a ndc::models::ObjectField, TypeMappingValidationError> {
    ndc_type
        .fields
        .get(column)
        .ok_or(TypeMappingValidationError::UnknownTargetColumn {
            field_name: field_name.clone(),
            column_name: column.to_string(),
        })
}

pub struct TypeMappingToResolve<'a> {
    pub type_name: &'a Qualified<CustomTypeName>,
    pub ndc_object_type_name: &'a str,
    pub ndc_object_type: &'a ndc::models::ObjectType,
}

pub fn resolve_type_mappings<'a, TIter: Iterator<Item = &'a TypeMappingToResolve<'a>>>(
    mut mappings_to_resolve: TIter,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, &commands::TypeMapping>,
    all_type_representations: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    ndc_object_types: &BTreeMap<String, ndc::models::ObjectType>,
) -> Result<BTreeMap<Qualified<CustomTypeName>, TypeMapping>, TypeMappingValidationError> {
    mappings_to_resolve.try_fold(
        BTreeMap::<Qualified<CustomTypeName>, TypeMapping>::new(),
        |resolved_type_mappings, type_mapping_to_resolve| {
            resolve_type_mapping(
                type_mapping_to_resolve,
                type_mappings,
                all_type_representations,
                ndc_object_types,
                resolved_type_mappings,
            )
        },
    )
}

fn resolve_type_mapping(
    mapping_to_resolve: &TypeMappingToResolve,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, &commands::TypeMapping>,
    all_type_representations: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    ndc_object_types: &BTreeMap<String, ndc::models::ObjectType>,
    mut resolved_type_mappings: BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<BTreeMap<Qualified<CustomTypeName>, TypeMapping>, TypeMappingValidationError> {
    // TODO: What about if we accidentally try to resolve type x into two different ndc types?
    // We need to detect that and fail. Currently we'll just use the first silently.
    if resolved_type_mappings.contains_key(mapping_to_resolve.type_name) {
        return Ok(resolved_type_mappings); // We've already resolved this type's mappings
    }

    let type_representation = all_type_representations
        .get(mapping_to_resolve.type_name)
        .ok_or_else(|| TypeMappingValidationError::UnknownSourceType {
            type_name: mapping_to_resolve.type_name.clone(),
        })?;

    let object_fields =
        if let TypeRepresentation::Object(object_type_representation) = type_representation {
            &object_type_representation.fields
        } else {
            return Err(
                TypeMappingValidationError::IncompatibleTypeMappingDefinition {
                    type_name: mapping_to_resolve.type_name.clone(),
                },
            );
        };

    // Walk all the fields in the ObjectType, if there's a mapping for the field
    // use it, otherwise assume the destination column is the same name as the field.
    // At the end, if there are any mappings left over, these are invalid as they do not
    // exist in the actual ObjectType.
    let mut unconsumed_field_mappings = type_mappings
        .get(mapping_to_resolve.type_name)
        .map_or(HashMap::new(), |type_mapping| {
            HashMap::from_iter(type_mapping.field_mapping.iter())
        });
    let mut resolved_field_mappings = BTreeMap::new();
    let mut nested_field_types_to_resolve = HashMap::new();
    for (field_name, field_definition) in object_fields {
        let resolved_field_mapping_column =
            if let Some(field_mapping) = unconsumed_field_mappings.remove(field_name) {
                &field_mapping.column
            } else {
                // If no mapping is defined for a field, implicitly create a mapping
                // with the same column name as the field.
                &field_name.0
            };
        let source_column = get_column(
            mapping_to_resolve.ndc_object_type,
            field_name,
            resolved_field_mapping_column,
        )?;
        let resolved_field_mapping = FieldMapping {
            column: resolved_field_mapping_column.clone(),
            column_type: source_column.r#type.clone(),
        };

        let existing_mapping =
            resolved_field_mappings.insert(field_name.clone(), resolved_field_mapping);
        if existing_mapping.is_some() {
            return Err(TypeMappingValidationError::DuplicateFieldMapping {
                type_name: mapping_to_resolve.type_name.clone(),
                field_name: field_name.clone(),
            });
        }
        nested_field_types_to_resolve.insert(
            &field_definition.field_type,
            (
                field_name,
                &source_column.r#type,
                resolved_field_mapping_column,
            ),
        );
    }
    // If any unconsumed field mappings, these do not exist in the actual ObjectType
    let unconsumed_field_names = unconsumed_field_mappings
        .into_keys()
        .cloned()
        .collect::<Vec<_>>();
    if !unconsumed_field_names.is_empty() {
        return Err(TypeMappingValidationError::UnknownSourceFields {
            type_name: mapping_to_resolve.type_name.clone(),
            field_names: unconsumed_field_names,
        });
    }

    resolved_type_mappings.insert(
        mapping_to_resolve.type_name.clone(),
        TypeMapping::Object {
            field_mappings: resolved_field_mappings,
        },
    );

    // For each field in the ObjectType, if that field is using an ObjectType in its type,
    // resolve the type mappings for that ObjectType too
    for (type_reference, (field_name, ndc_field_type, ndc_field_name)) in
        nested_field_types_to_resolve
    {
        if let Some(object_type_name) =
            get_underlying_object_type_or_unknown_type(type_reference, all_type_representations)
                .map_err(
                    |unknown_type| TypeMappingValidationError::UnknownFieldType {
                        type_name: mapping_to_resolve.type_name.clone(),
                        field_name: field_name.clone(),
                        unknown_field_type_name: unknown_type.clone(),
                    },
                )?
        {
            let underlying_ndc_field_named_type = get_underlying_named_type(ndc_field_type);
            let ndc_field_object_type = ndc_object_types
                .get(underlying_ndc_field_named_type)
                .ok_or_else(|| TypeMappingValidationError::UnknownNdcFieldType {
                    type_name: mapping_to_resolve.type_name.clone(),
                    field_name: field_name.clone(),
                    ndc_type_name: mapping_to_resolve.ndc_object_type_name.into(),
                    ndc_field_name: ndc_field_name.clone(),
                    unknown_ndc_field_type_name: underlying_ndc_field_named_type.into(),
                })?;

            let field_type_mapping_to_resolve = TypeMappingToResolve {
                type_name: object_type_name,
                ndc_object_type_name: underlying_ndc_field_named_type,
                ndc_object_type: ndc_field_object_type,
            };
            resolved_type_mappings = resolve_type_mapping(
                &field_type_mapping_to_resolve,
                type_mappings,
                all_type_representations,
                ndc_object_types,
                resolved_type_mappings,
            )?;
        }
    }

    Ok(resolved_type_mappings)
}

// Get the underlying object type by resolving Custom ObjectType, Array and
// Nullable container types
pub fn get_underlying_object_type(
    output_type: &QualifiedTypeReference,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<Option<Qualified<CustomTypeName>>, Error> {
    get_underlying_object_type_or_unknown_type(output_type, types)
        .map(|opt| opt.cloned())
        .map_err(|custom_type_name| Error::UnknownDataType {
            data_type: custom_type_name.clone(),
        })
}

pub fn get_underlying_object_type_or_unknown_type<'a>(
    output_type: &'a QualifiedTypeReference,
    types: &'a HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<Option<&'a Qualified<CustomTypeName>>, Qualified<CustomTypeName>> {
    match &output_type.underlying_type {
        QualifiedBaseType::List(output_type) => {
            get_underlying_object_type_or_unknown_type(output_type, types)
        }
        QualifiedBaseType::Named(type_name) => match type_name {
            QualifiedTypeName::Inbuilt(_) => Ok(None),
            QualifiedTypeName::Custom(custom_type_name) => {
                let type_representation = types
                    .get(custom_type_name)
                    .ok_or_else(|| custom_type_name.clone())?;
                match type_representation {
                    TypeRepresentation::ScalarType { .. } => Ok(None),
                    TypeRepresentation::Object { .. } => Ok(Some(custom_type_name)),
                }
            }
        },
    }
}

pub fn resolve_output_type_permission(
    type_representation: &mut TypeRepresentation,
    type_permissions: &TypePermissionsV1,
) -> Result<(), Error> {
    match type_representation {
        TypeRepresentation::ScalarType { .. } => Err(Error::UnsupportedTypeInOutputPermissions {
            type_name: type_permissions.type_name.clone(),
        }),
        TypeRepresentation::Object(object_type_representation) => {
            // validate all the fields definied in output permissions actually
            // exist in this type definition
            for type_permission in &type_permissions.permissions {
                if let Some(output) = &type_permission.output {
                    for field_name in output.allowed_fields.iter() {
                        if !object_type_representation.fields.contains_key(field_name) {
                            return Err(Error::UnknownFieldInOutputPermissionsDefinition {
                                field_name: field_name.clone(),
                                type_name: type_permissions.type_name.clone(),
                            });
                        }
                    }
                    if object_type_representation
                        .type_permissions
                        .insert(type_permission.role.clone(), output.clone())
                        .is_some()
                    {
                        return Err(Error::DuplicateOutputTypePermissions {
                            type_name: type_permissions.type_name.clone(),
                        });
                    }
                }
            }
            Ok(())
        }
    }
}

// helper function for creating graphql compliant name
pub fn mk_name(name: &str) -> Result<ast::Name, Error> {
    ast::Name::from_str(name).map_err(|_| Error::InvalidGraphQlName {
        name: name.to_string(),
    })
}
