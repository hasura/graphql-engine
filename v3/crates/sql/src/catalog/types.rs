use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::{Debug, Display},
    sync::Arc,
};

use indexmap::IndexMap;
use metadata_resolve::{
    self as resolved, Qualified, QualifiedTypeReference, ScalarTypeRepresentation,
    ValueRepresentation,
};
use open_dds::{identifier::SubgraphName, types::CustomTypeName};

mod datafusion {
    pub(super) use datafusion::arrow::datatypes::{
        DataType, Field, Schema, SchemaBuilder, SchemaRef, TimeUnit,
    };
}

mod open_dd {
    pub(super) use open_dds::types::FieldName;
}

use datafusion::DataType;
use serde::{Deserialize, Serialize};
use thiserror::Error;

type SupportedScalar = (NormalizedType, datafusion::DataType);

#[derive(Error, Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum UnsupportedScalar {
    #[error("Multiple NDC type representations found for scalar type '{0}'")]
    MultipleNdcTypeRepresentations(CustomTypeName, HashSet<ValueRepresentation>),
    #[error("No NDC representation found for scalar type '{0}'")]
    NoNdcRepresentation(CustomTypeName),
    #[error("Unsupported NDC type representation for scalar type '{0}': {1:?}")]
    NdcRepresentationNotSupported(CustomTypeName, ndc_models::TypeRepresentation),
}

type Scalar = Result<SupportedScalar, UnsupportedScalar>;

pub fn resolve_scalar_type(
    scalar_type_name: &Qualified<CustomTypeName>,
    representation: &ScalarTypeRepresentation,
) -> Scalar {
    let representations: HashSet<ValueRepresentation> =
        representation.representations.values().cloned().collect();
    let mut iter = representations.into_iter();
    let ndc_representation = match iter.next() {
        Some(value_representation) => {
            // more than one representation
            if iter.next().is_some() {
                return Err(UnsupportedScalar::MultipleNdcTypeRepresentations(
                    scalar_type_name.name.clone(),
                    representation.representations.values().cloned().collect(),
                ));
            }
            match value_representation {
                ValueRepresentation::FromDataConnectorSchema(representation) => {
                    Some(representation)
                }
                // if it is json, let's try and infer it from name
                ValueRepresentation::AssumeJson => infer_ndc_representation(&scalar_type_name.name),
            }
        }
        // if no representation, let's try and infer it from name
        None => infer_ndc_representation(&scalar_type_name.name),
    };

    let ndc_representation = ndc_representation
        .ok_or_else(|| UnsupportedScalar::NoNdcRepresentation(scalar_type_name.name.clone()))?;
    ndc_representation_to_datatype(&ndc_representation).ok_or_else(|| {
        UnsupportedScalar::NdcRepresentationNotSupported(
            scalar_type_name.name.clone(),
            ndc_representation.clone(),
        )
    })
}

pub fn resolve_scalar_types(
    metadata: &resolved::Metadata,
) -> HashMap<Qualified<CustomTypeName>, Scalar> {
    let mut custom_scalars: HashMap<Qualified<CustomTypeName>, Scalar> = HashMap::new();
    for (scalar_type_name, representation) in &metadata.scalar_types {
        let scalar = resolve_scalar_type(scalar_type_name, representation);
        custom_scalars.insert(scalar_type_name.clone(), scalar);
    }
    custom_scalars
}

pub struct TypeRegistry {
    custom_scalars: HashMap<Qualified<CustomTypeName>, Scalar>,
    struct_types: HashMap<Qualified<CustomTypeName>, Struct>,
    default_schema: Option<SubgraphName>,
}

#[derive(Error, Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum UnsupportedType {
    #[error("Unsupported scalar type: {0}")]
    Scalar(#[from] UnsupportedScalar),
    #[error("Unsupported object type: {0}")]
    Object(#[from] UnsupportedObject),
    #[error("type not found in internal registry")]
    InternalNotFound,
}

impl TypeRegistry {
    pub fn build_type_registry(metadata: &resolved::Metadata) -> Self {
        // build a default schema by checking if the object types are spread across more than one
        // subgraph
        let default_schema = {
            let subgraphs: HashSet<_> = metadata
                .object_types
                .keys()
                .map(|object_type_name| &object_type_name.subgraph)
                .collect();
            if subgraphs.len() == 1 {
                subgraphs.into_iter().next().cloned()
            } else {
                None
            }
        };

        let custom_scalars = resolve_scalar_types(metadata);

        let mut struct_types: HashMap<Qualified<CustomTypeName>, Struct> = HashMap::new();

        for (object_type_name, object_type) in &metadata.object_types {
            let struct_type = struct_type(
                &default_schema,
                metadata,
                &custom_scalars,
                object_type_name,
                object_type,
                &BTreeSet::from_iter([object_type_name.clone()]),
            );
            struct_types.insert(object_type_name.clone(), struct_type);
        }

        Self {
            custom_scalars,
            struct_types,
            default_schema,
        }
    }

    pub(crate) fn get_datafusion_type(
        &self,
        base_type: &resolved::QualifiedBaseType,
    ) -> Result<(NormalizedType, datafusion::DataType), UnsupportedType> {
        use resolved::{QualifiedBaseType, QualifiedTypeName};
        match base_type {
            QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(inbuilt_type)) => {
                Ok(built_in_type(inbuilt_type))
            }
            QualifiedBaseType::Named(QualifiedTypeName::Custom(custom_type_name)) => {
                if let Some(scalar_result) = self.custom_scalars.get(custom_type_name) {
                    Ok(scalar_result.clone()?)
                } else if let Some(struct_result) = self.struct_types.get(custom_type_name) {
                    let struct_type = struct_result.as_ref().map_err(std::clone::Clone::clone)?;
                    let datafusion_type =
                        datafusion::DataType::Struct(struct_type.table_schema().fields.clone());
                    Ok((
                        NormalizedType::Named(NormalizedTypeNamed::Struct(
                            struct_type.name().clone(),
                        )),
                        datafusion_type,
                    ))
                } else {
                    Err(UnsupportedType::InternalNotFound)
                }
            }
            QualifiedBaseType::List(element_type) => {
                let (normalized_type, data_type) =
                    self.get_datafusion_type(&element_type.underlying_type)?;
                let datafusion_type = datafusion::DataType::List(Arc::new(datafusion::Field::new(
                    "element",
                    data_type,
                    element_type.nullable,
                )));
                Ok((
                    NormalizedType::List(Box::new(NormalizedTypeReference {
                        underlying_type: normalized_type,
                        nullable: element_type.nullable,
                    })),
                    datafusion_type,
                ))
            }
        }
    }

    pub(crate) fn get_object(
        &self,
        type_name: &resolved::Qualified<CustomTypeName>,
    ) -> Option<&Struct> {
        self.struct_types.get(type_name)
        // ) -> Result<&StructType, UnsupportedObject> {
        // match self.struct_types.get(type_name) {
        //     None => Err(UnsupportedObject::InternalNotFound),
        //     Some(Ok(r#struct)) => Ok(r#struct),
        //     Some(Err(unsupported)) => Err(unsupported.clone()),
        // }
    }

    pub(crate) fn struct_types(&self) -> &HashMap<Qualified<CustomTypeName>, Struct> {
        &self.struct_types
    }

    pub(crate) fn default_schema(&self) -> Option<&SubgraphName> {
        self.default_schema.as_ref()
    }

    pub fn custom_scalars(&self) -> &HashMap<Qualified<CustomTypeName>, Scalar> {
        &self.custom_scalars
    }
}

// These set of types are used to capture an opendd type that has been validated as part of the sql
// layer
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct NormalizedTypeReference {
    pub underlying_type: NormalizedType,
    pub nullable: bool,
}

impl Display for NormalizedTypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nullability = if self.nullable { "" } else { " NOT NULL" };
        write!(f, "{}{}", self.underlying_type, nullability)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum NormalizedType {
    Named(NormalizedTypeNamed),
    List(Box<NormalizedTypeReference>),
}

impl Display for NormalizedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NormalizedType::Named(named) => write!(f, "{named}"),
            NormalizedType::List(list) => {
                write!(f, "ARRAY<{list}>")
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum NormalizedTypeNamed {
    Scalar(datafusion::DataType),
    Struct(StructTypeName),
}

impl Display for NormalizedTypeNamed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            NormalizedTypeNamed::Scalar(s) => display_data_type(s),
            NormalizedTypeNamed::Struct(s) => s.0.clone(),
        };
        write!(f, "{name}")
    }
}

// Name of a struct type
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct StructTypeName(pub String);

impl StructTypeName {
    fn new(
        default_schema: &Option<SubgraphName>,
        object_type_name: &Qualified<CustomTypeName>,
    ) -> Self {
        let name = if Some(&object_type_name.subgraph) == default_schema.as_ref() {
            object_type_name.name.to_string()
        } else {
            format!("{}_{}", object_type_name.subgraph, object_type_name.name)
        };
        StructTypeName(name)
    }
}

#[derive(Clone)]
pub struct UnsupportedField {
    pub field_type: QualifiedTypeReference,
    pub reason: UnsupportedType,
}

#[derive(Clone)]
pub(crate) struct StructType {
    pub name: StructTypeName,
    pub description: Option<String>,
    pub fields: IndexMap<open_dd::FieldName, StructTypeField>,
    pub unsupported_fields: IndexMap<open_dd::FieldName, UnsupportedField>,
    pub schema: datafusion::SchemaRef,
}

impl StructType {
    pub fn table_schema(&self) -> &datafusion::SchemaRef {
        &self.schema
    }

    pub fn name(&self) -> &StructTypeName {
        &self.name
    }

    pub fn description(&self) -> Option<&String> {
        self.description.as_ref()
    }

    pub(crate) fn fields(&self) -> &IndexMap<open_dd::FieldName, StructTypeField> {
        &self.fields
    }
}

#[derive(Clone)]
pub(crate) struct StructTypeField {
    pub _name: open_dd::FieldName,
    pub description: Option<String>,
    pub data_type: datafusion::DataType,
    pub is_nullable: bool,
    pub normalized_type: NormalizedType,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Error)]
pub enum UnsupportedObject {
    #[error("Recursive object type reference detected for type '{0}' through path: {1}")]
    Recursive(CustomTypeName, String),
}

type Struct = Result<StructType, UnsupportedObject>;

fn struct_type(
    default_schema: &Option<SubgraphName>,
    metadata: &resolved::Metadata,
    custom_scalars: &HashMap<Qualified<CustomTypeName>, Scalar>,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &resolved::ObjectTypeWithRelationships,
    disallowed_object_types: &BTreeSet<Qualified<CustomTypeName>>,
) -> Result<StructType, UnsupportedObject> {
    let object_type_fields = &object_type.object_type.fields;
    let mut struct_fields = IndexMap::new();
    let mut unsupported_fields = IndexMap::new();
    let mut builder = datafusion::SchemaBuilder::new();
    for (field_name, field_definition) in object_type_fields {
        let field_type = to_arrow_type(
            default_schema,
            metadata,
            custom_scalars,
            &field_definition.field_type.underlying_type,
            disallowed_object_types,
        );
        match field_type {
            Err(UnsupportedType::Object(UnsupportedObject::Recursive(type_name, path))) => {
                // We are in a nested context if and only if the disallowed_object_types
                // stack/set has size > 1, because there are only two cases:
                // 1. we added a new unseen type name to the set, which must have happened
                //    when considering a nested object type, or
                // 2. we re-added an already-seen type name, in which case the set size
                //    would have remained unchanged. But this case would have been
                //    short-circuited by the recursive type check anyway.
                let is_nested = disallowed_object_types.len() > 1;
                if is_nested {
                    return Err(UnsupportedObject::Recursive(type_name, path));
                }
            }
            Err(unsupported_type) => {
                let unsupported_field = UnsupportedField {
                    field_type: field_definition.field_type.clone(),
                    reason: unsupported_type,
                };
                unsupported_fields.insert(field_name.clone(), unsupported_field);
            }
            Ok((normalized, flat)) => {
                builder.push(datafusion::Field::new(
                    field_name.to_string(),
                    flat.clone(),
                    field_definition.field_type.nullable,
                ));

                // let description = if let Some(ndc_models::TypeRepresentation::Enum { one_of }) =
                //     type_representation
                // {
                //     // TODO: Instead of stuffing the possible enum values in description,
                //     // surface them in the metadata tables.
                //     Some(
                //         field_definition
                //             .description
                //             .clone()
                //             .unwrap_or_else(String::new)
                //             + &format!(" Possible values: {}", one_of.join(", ")),
                //     )
                // } else {
                //     field_definition.description.clone()
                // };
                let object_type_field = StructTypeField {
                    _name: field_name.clone(),
                    description: field_definition.description.clone(),
                    data_type: flat,
                    is_nullable: field_definition.field_type.nullable,
                    normalized_type: normalized,
                };
                struct_fields.insert(field_name.clone(), object_type_field);
            }
        }
    }

    let struct_type = StructType {
        name: StructTypeName::new(default_schema, object_type_name),
        description: object_type.object_type.description.clone(),
        fields: struct_fields,
        unsupported_fields,
        schema: datafusion::SchemaRef::new(datafusion::Schema::new(builder.finish().fields)),
    };

    // struct_types.insert(object_type_name.clone(), struct_type.clone());

    Ok(struct_type)
}

fn built_in_type(ty: &open_dds::types::InbuiltType) -> (NormalizedType, datafusion::DataType) {
    let scalar = match ty {
        open_dds::types::InbuiltType::ID | open_dds::types::InbuiltType::String => {
            datafusion::DataType::Utf8
        }
        open_dds::types::InbuiltType::Int => datafusion::DataType::Int32,
        open_dds::types::InbuiltType::Float => datafusion::DataType::Float32,
        open_dds::types::InbuiltType::Boolean => datafusion::DataType::Boolean,
    };
    (
        NormalizedType::Named(NormalizedTypeNamed::Scalar(scalar.clone())),
        scalar,
    )
}

fn infer_ndc_representation(name: &CustomTypeName) -> Option<ndc_models::TypeRepresentation> {
    match name.to_string().to_lowercase().as_str() {
        "bool" => Some(ndc_models::TypeRepresentation::Boolean),
        "int8" => Some(ndc_models::TypeRepresentation::Int8),
        "int16" => Some(ndc_models::TypeRepresentation::Int16),
        "int32" => Some(ndc_models::TypeRepresentation::Int32),
        "int64" => Some(ndc_models::TypeRepresentation::Int64),
        "float32" => Some(ndc_models::TypeRepresentation::Float32),
        "float64" |
        // BigDecimal128 is not supported by arrow.
        "bigdecimal" => Some(ndc_models::TypeRepresentation::Float64),
        "varchar" | "text" => Some(ndc_models::TypeRepresentation::String),
        "timestamp" | "timestamptz" => Some(ndc_models::TypeRepresentation::Timestamp),
        _ => None
    }
}

fn ndc_representation_to_datatype(
    representation: &ndc_models::TypeRepresentation,
) -> Option<(NormalizedType, datafusion::DataType)> {
    let scalar = match representation {
        ndc_models::TypeRepresentation::Boolean => Some(datafusion::DataType::Boolean),
        ndc_models::TypeRepresentation::Int8 => Some(datafusion::DataType::Int8),
        ndc_models::TypeRepresentation::Int16 => Some(datafusion::DataType::Int16),
        ndc_models::TypeRepresentation::Int32 => Some(datafusion::DataType::Int32),
        ndc_models::TypeRepresentation::Int64 => Some(datafusion::DataType::Int64),
        ndc_models::TypeRepresentation::Float32 => Some(datafusion::DataType::Float32),
        ndc_models::TypeRepresentation::Float64 |
        // BigDecimal128 is not supported by arrow.
        ndc_models::TypeRepresentation::BigDecimal => Some(datafusion::DataType::Float64),
        ndc_models::TypeRepresentation::String |
        // Can't do anything better for BigInteger, so we just use String.
        ndc_models::TypeRepresentation::BigInteger |
        ndc_models::TypeRepresentation::UUID |
        ndc_models::TypeRepresentation::Enum { .. } => Some(datafusion::DataType::Utf8),
        ndc_models::TypeRepresentation::Date => Some(datafusion::DataType::Date32),
        ndc_models::TypeRepresentation::Timestamp |
        ndc_models::TypeRepresentation::TimestampTZ => Some(
            datafusion::DataType::Timestamp(datafusion::TimeUnit::Microsecond, None),
        ),
        _ => None,
    }?;
    Some((
        NormalizedType::Named(NormalizedTypeNamed::Scalar(scalar.clone())),
        scalar,
    ))
}

/// Converts an opendd type to an arrow type.
#[allow(clippy::match_same_arms)]
fn to_arrow_type(
    default_schema: &Option<SubgraphName>,
    metadata: &resolved::Metadata,
    custom_scalars: &HashMap<Qualified<CustomTypeName>, Scalar>,
    ty: &resolved::QualifiedBaseType,
    disallowed_object_types: &BTreeSet<Qualified<CustomTypeName>>,
    // ) -> GeneratedArrowType {
) -> Result<(NormalizedType, datafusion::DataType), UnsupportedType> {
    match &ty {
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Inbuilt(inbuilt_type)) => {
            Ok(built_in_type(inbuilt_type))
        }
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Custom(custom_type)) => {
            // check if the custom type is a scalar
            if let Some(scalar) = custom_scalars.get(custom_type) {
                Ok(scalar.clone()?)
            } else if let Some(object_type) = metadata.object_types.get(custom_type) {
                if disallowed_object_types.contains(custom_type) {
                    let path = disallowed_object_types
                        .iter()
                        .map(|t| t.name.to_string())
                        .collect::<Vec<_>>()
                        .join(" -> ");
                    // If we have seen this type name before, then we have a recursive
                    // object type. Recursive structs are not supported by datafusion,
                    // so we cut off the recursion here and omit this field.
                    return Err(UnsupportedType::Object(UnsupportedObject::Recursive(
                        custom_type.name.clone(),
                        path,
                    )));
                }

                let mut disallowed_object_types = disallowed_object_types.clone();
                disallowed_object_types.insert(custom_type.clone());

                let struct_type = struct_type(
                    default_schema,
                    metadata,
                    custom_scalars,
                    custom_type,
                    object_type,
                    &disallowed_object_types,
                )?;

                let flat = datafusion::DataType::Struct(struct_type.table_schema().fields.clone());
                Ok((
                    NormalizedType::Named(NormalizedTypeNamed::Struct(struct_type.name().clone())),
                    flat,
                ))
            } else {
                Err(UnsupportedType::InternalNotFound)
            }
        }
        resolved::QualifiedBaseType::List(element_type) => {
            let (normalized, flat) = to_arrow_type(
                default_schema,
                metadata,
                custom_scalars,
                &element_type.underlying_type,
                disallowed_object_types,
            )?;
            let flat = datafusion::DataType::List(Arc::new(datafusion::Field::new(
                "element",
                flat,
                element_type.nullable,
            )));
            let normalized = NormalizedType::List(Box::new(NormalizedTypeReference {
                underlying_type: normalized,
                nullable: element_type.nullable,
            }));
            Ok((normalized, flat))
        }
    }
}

// Displays a datafusion type in some sql format, maybe we should follow postgres's syntax
pub fn display_data_type(data_type: &DataType) -> String {
    match data_type {
        DataType::Boolean => "BOOL".to_string(),
        DataType::Int8 | DataType::UInt8 => "INT8".to_string(),
        DataType::Int16 | DataType::UInt16 => "INT16".to_string(),
        DataType::Int32 | DataType::UInt32 => "INT32".to_string(),
        DataType::Int64 | DataType::UInt64 => "INT64".to_string(),
        DataType::Float16 => "FLOAT16".to_string(),
        DataType::Float32 => "FLOAT32".to_string(),
        DataType::Float64 => "FLOAT64".to_string(),
        DataType::Utf8 | DataType::LargeUtf8 => "STRING".to_string(),
        DataType::Binary | DataType::LargeBinary => "BYTES".to_string(),
        DataType::Date32 | DataType::Date64 => "DATE".to_string(),
        DataType::Timestamp(_, _) => "TIMESTAMP".to_string(),
        DataType::Time32(_) | DataType::Time64(_) => "TIME".to_string(),
        DataType::Decimal128(precision, scale) => format!("NUMERIC({precision}, {scale})"),
        DataType::Decimal256(precision, scale) => format!("BIGNUMERIC({precision}, {scale})"),
        DataType::Struct(fields) => {
            let field_types: Vec<String> = fields
                .iter()
                .map(|f| {
                    let type_str = display_data_type(f.data_type());
                    if f.is_nullable() {
                        format!("{} {}", f.name(), type_str)
                    } else {
                        format!("{} {} NOT NULL", f.name(), type_str)
                    }
                })
                .collect();
            format!("STRUCT<{}>", field_types.join(", "))
        }
        DataType::List(field) => {
            format!("ARRAY<{}>", display_data_type(field.data_type()))
        }
        t => format!("{t:?}"),
    }
}
