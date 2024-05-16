use convert_case::{Case, Casing};
use darling::{FromAttributes, FromField, FromMeta};
use syn::{self, DeriveInput};

use crate::MacroResult;

/// JSON schema attributes
#[derive(Default, FromMeta)]
#[darling(default)]
struct JsonSchemaOpts {
    rename: Option<String>,
    title: Option<String>,
    example: Option<syn::Path>,
}

/// Struct container attributes
#[derive(FromAttributes, Default)]
#[darling(default, attributes(opendd))]
struct StructOpts {
    #[darling(default)]
    json_schema: JsonSchemaOpts,
}

/// Struct field attributes
#[derive(FromField, Default)]
#[darling(default, attributes(opendd))]
struct FieldOpts {
    default: Option<bool>,
    rename: Option<String>,
    alias: Option<String>,
    #[darling(default)]
    json_schema: JsonSchemaFieldOpts,
}

/// JSON schema field attributes
#[derive(Default, FromMeta)]
#[darling(default)]
struct JsonSchemaFieldOpts {
    default_exp: Option<syn::Expr>,
}

/// Enum container attributes
#[derive(FromAttributes, Default)]
#[darling(default, attributes(opendd))]
struct EnumOpts {
    as_kind: Option<bool>,
    as_versioned_internally_tagged: Option<bool>,
    as_versioned_with_definition: Option<bool>,
    untagged_with_kind: Option<bool>,
    #[darling(default)]
    json_schema: JsonSchemaOpts,
}

/// Variant attributes
#[derive(FromAttributes, Default)]
#[darling(default, attributes(opendd))]
struct VariantOpts {
    rename: Option<String>,
    alias: Option<String>,
}

pub struct Container<'a> {
    pub json_schema_metadata: JsonSchemaMetadata,
    pub data: Data<'a>,
}

pub struct JsonSchemaMetadata {
    pub schema_name: String,
    pub title: String,
    pub description: Option<String>,
    pub example: Option<syn::Path>,
}

impl<'a> Container<'a> {
    pub fn from_derive_input(input: &'a DeriveInput) -> MacroResult<Self> {
        let (doc_title, doc_description) =
            crate::helpers::get_title_and_desc_from_doc(&input.attrs);
        let (json_schema_opts, data) = match &input.data {
            syn::Data::Struct(data) => {
                let struct_opts = StructOpts::from_attributes(&input.attrs)?;
                let struct_data = StructData::from_fields(&data.fields)?;
                (struct_opts.json_schema, Data::Struct(struct_data))
            }
            syn::Data::Enum(data) => {
                let enum_opts = EnumOpts::from_attributes(&input.attrs)?;
                let enum_data = EnumData::from_data(data, &enum_opts)?;
                (enum_opts.json_schema, Data::Enum(enum_data))
            }
            syn::Data::Union(_) => {
                return Err(
                    syn::Error::new_spanned(input, "only enums and structs are supported").into(),
                );
            }
        };
        let schema_name = json_schema_opts
            .rename
            .unwrap_or_else(|| input.ident.to_string());
        let schema_title = json_schema_opts
            .title
            .or(doc_title)
            .unwrap_or(schema_name.to_string());
        let schema_example = json_schema_opts.example;
        let json_schema_metadata = JsonSchemaMetadata {
            schema_name,
            title: schema_title,
            description: doc_description,
            example: schema_example,
        };
        Ok(Self {
            json_schema_metadata,
            data,
        })
    }
}

pub enum Data<'a> {
    Struct(StructData<'a>),
    Enum(EnumData<'a>),
}

pub enum StructData<'a> {
    Newtype(&'a syn::Field),
    Named(Vec<NamedField<'a>>),
}

impl<'a> StructData<'a> {
    fn from_fields(fields: &'a syn::Fields) -> MacroResult<Self> {
        match fields {
            syn::Fields::Named(named_fields) => {
                let named_fields = named_fields
                    .named
                    .iter()
                    .map(NamedField::from_field)
                    .collect::<MacroResult<_>>()?;
                Ok(Self::Named(named_fields))
            }
            syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                Ok(Self::Newtype(&fields.unnamed[0]))
            }
            _ => Err(syn::Error::new_spanned(
                fields,
                "only structs with named fields or newtype structs are supported",
            )
            .into()),
        }
    }
}

pub struct NamedField<'a> {
    pub field_name: &'a syn::Ident,
    pub field_type: &'a syn::Type,
    pub renamed_field: String,
    pub field_alias: Option<String>,
    pub is_default: bool,
    pub is_optional: bool,
    pub default_exp: Option<syn::Expr>,
    pub description: Option<String>,
}

impl<'a> NamedField<'a> {
    fn from_field(field: &'a syn::Field) -> MacroResult<Self> {
        let (_, description) = super::helpers::get_title_and_desc_from_doc(&field.attrs);
        let field_type = &field.ty;
        let field_opts = FieldOpts::from_field(field)?;
        let field_name = field
            .ident
            .as_ref()
            .ok_or_else(|| syn::Error::new_spanned(field, "field does not have an identifier"))?;
        let renamed_field = field_opts
            .rename
            .unwrap_or_else(|| field_name.to_string().to_case(Case::Camel));
        let field_alias = field_opts.alias;
        let is_default = field_opts.default.unwrap_or(false);
        let is_optional = is_option_type(&field.ty);
        let default_exp = field_opts.json_schema.default_exp;
        Ok(Self {
            field_name,
            field_type,
            renamed_field,
            field_alias,
            is_default,
            is_optional,
            default_exp,
            description,
        })
    }
}

pub enum EnumData<'a> {
    DefaultImpl,
    Impl(EnumImplStyle, Vec<EnumVariant<'a>>),
}

impl<'a> EnumData<'a> {
    fn from_data(data: &'a syn::DataEnum, options: &EnumOpts) -> MacroResult<Self> {
        match EnumImplStyle::from_opts(options) {
            Some(impl_style) => {
                let variants = data
                    .variants
                    .iter()
                    .map(|variant| EnumVariant::from_variant(variant, &impl_style))
                    .collect::<MacroResult<_>>()?;
                Ok(Self::Impl(impl_style, variants))
            }
            None => Ok(Self::DefaultImpl),
        }
    }
}

pub enum EnumImplStyle {
    UntaggedWithKind,
    Tagged(Tagged),
}

impl EnumImplStyle {
    fn from_opts(opts: &EnumOpts) -> Option<Self> {
        if opts.as_kind.unwrap_or(false) {
            Some(Self::Tagged(Tagged::KindInternal))
        } else if opts.as_versioned_internally_tagged.unwrap_or(false) {
            Some(Self::Tagged(Tagged::VersionInternal))
        } else if opts.as_versioned_with_definition.unwrap_or(false) {
            Some(Self::Tagged(Tagged::VersionWithDefinition))
        } else if opts.untagged_with_kind.unwrap_or(false) {
            Some(Self::UntaggedWithKind)
        } else {
            None
        }
    }
}

pub enum Tagged {
    KindInternal,
    VersionInternal,
    VersionWithDefinition,
}

pub struct EnumVariant<'a> {
    pub name: &'a syn::Ident,
    pub renamed_variant: String,
    pub alias: Option<String>,
    pub field: &'a syn::Field,
}

impl<'a> EnumVariant<'a> {
    fn from_variant(variant: &'a syn::Variant, style: &EnumImplStyle) -> MacroResult<Self> {
        let variant_name = &variant.ident;
        let variant_opts = VariantOpts::from_attributes(&variant.attrs)?;
        let renamed_variant = variant_opts.rename.unwrap_or_else(|| {
            match style {
                EnumImplStyle::UntaggedWithKind => variant_name.to_string(),
                EnumImplStyle::Tagged(tag_style) => match tag_style {
                    // Preserve casing for kinded enums
                    Tagged::KindInternal => variant_name.to_string(),
                    // Use camel-casing for versioned enums
                    Tagged::VersionInternal => variant_name.to_string().to_case(Case::Camel),
                    Tagged::VersionWithDefinition => variant_name.to_string().to_case(Case::Camel),
                },
            }
        });
        let alias = variant_opts.alias;
        let field = match &variant.fields {
            syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => &fields.unnamed[0],
            _ => {
                return Err(syn::Error::new_spanned(
                    variant,
                    "variants must have exactly one unnamed field",
                )
                .into())
            }
        };
        Ok(Self {
            name: variant_name,
            renamed_variant,
            alias,
            field,
        })
    }
}

/// Check whether the type is `Option<T>`
fn is_option_type(ty: &syn::Type) -> bool {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];
            if segment.ident == "Option" {
                return true;
            }
        }
    }
    false
}

/// Derive macro implementation result
pub struct TraitImpls {
    pub deserialize: proc_macro2::TokenStream,
    pub json_schema: proc_macro2::TokenStream,
}
