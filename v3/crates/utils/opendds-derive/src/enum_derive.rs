use quote::quote;

use crate::container::*;
use crate::helpers;

/// Enum tag type to generate appropriate code for tagged enums
enum EnumTagType {
    Internal { tag: String },
    Adjacent { tag: String, content: String },
    External,
}

impl EnumTagType {
    fn new(tag_kind: &Tagged) -> Self {
        match tag_kind {
            Tagged::KindInternal => EnumTagType::Internal {
                tag: "kind".to_string(),
            },
            Tagged::VersionInternal => EnumTagType::Internal {
                tag: "version".to_string(),
            },
            Tagged::VersionWithDefinition => EnumTagType::Adjacent {
                tag: "version".to_string(),
                content: "definition".to_string(),
            },
            Tagged::External => EnumTagType::External,
        }
    }

    fn generate_tag(&self) -> Option<String> {
        match self {
            EnumTagType::Internal { tag } | EnumTagType::Adjacent { tag, content: _ } => {
                Some(tag.to_string())
            }
            EnumTagType::External => None,
        }
    }
}

pub fn impl_opendd_enum(impl_style: EnumImplStyle, variants: &[EnumVariant<'_>]) -> TraitImpls {
    let (read_tag_value_str_exp, deserialize_exp, json_schema_expr) = match impl_style {
        EnumImplStyle::UntaggedWithKind => (
            impl_read_tag_value_str(Some("kind".to_string()), variants),
            impl_deserialize_as_untagged(variants),
            impl_json_schema_untagged(variants),
        ),
        EnumImplStyle::Tagged(tag_kind) => {
            let tag_type = EnumTagType::new(&tag_kind);
            (
                impl_read_tag_value_str(tag_type.generate_tag(), variants),
                impl_deserialize_as_tagged(variants, &tag_type),
                impl_json_schema_tagged(variants, &tag_type),
            )
        }
    };

    let impl_deserialize = quote! {
        let mut __object_map = match json {
            serde_json::Value::Object(map) => map,
            _ => {
                return Err(open_dds::traits::OpenDdDeserializeError  {
                    error: serde::de::Error::invalid_type(
                        serde::de::Unexpected::Other("not an object"),
                        &"object",
                    ),
                    path: jsonpath::JSONPath::new(),
                })
            }
        };

        #read_tag_value_str_exp

        #deserialize_exp
    };

    TraitImpls {
        deserialize: impl_deserialize,
        json_schema: json_schema_expr,
    }
}

/// Generate the code that reads the __tag_value_str
fn impl_read_tag_value_str(
    tag: Option<String>,
    variants: &[EnumVariant<'_>],
) -> proc_macro2::TokenStream {
    // If we have a tag property
    if let Some(tag) = tag {
        quote! {
            let __tag_value = __object_map.remove(#tag)
            .ok_or_else(|| open_dds::traits::OpenDdDeserializeError {
                error: serde::de::Error::missing_field(#tag),
                path: jsonpath::JSONPath::new(),
            })?;
            let __tag_value_str = __tag_value
                .as_str()
                .ok_or_else(|| open_dds::traits::OpenDdDeserializeError {
                    error: serde::de::Error::invalid_type(
                        serde::de::Unexpected::Other("not a string"),
                        &"string",
                    ),
                    path: jsonpath::JSONPath::new_key(#tag),
                })?;
        }
    }
    // We don't have a tag property, so we're using external tagging
    else {
        let variants_list = variants
            .iter()
            .filter_map(|var| {
                if var.hidden {
                    None
                } else {
                    Some(var.renamed_variant.clone())
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        let expected_variants_error =
            format!("object with only one of the following properties: {variants_list}");
        quote! {
            let mut __object_map_iter = __object_map.into_iter();
            let (__tag_value_string, __inner) = __object_map_iter.next().ok_or_else(||
                open_dds::traits::OpenDdDeserializeError  {
                    error: serde::de::Error::invalid_type(
                        serde::de::Unexpected::Other("found empty object"),
                        &#expected_variants_error,
                    ),
                    path: jsonpath::JSONPath::new(),
                }
            )?;
            if let Some(_) = __object_map_iter.next() {
                return Err(open_dds::traits::OpenDdDeserializeError  {
                    error: serde::de::Error::invalid_type(
                        serde::de::Unexpected::Other("found multiple object properties"),
                        &#expected_variants_error,
                    ),
                    path: jsonpath::JSONPath::new(),
                });
            }
            let __tag_value_str = __tag_value_string.as_str();
            let mut __object_map = match __inner {
                serde_json::Value::Object(map) => map,
                _ => {
                    return Err(open_dds::traits::OpenDdDeserializeError  {
                        error: serde::de::Error::invalid_type(
                            serde::de::Unexpected::Other("not an object"),
                            &"object",
                        ),
                        path: jsonpath::JSONPath::new_key(__tag_value_str),
                    })
                }
            };
        }
    }
}

/// Generate the implementation for untagged enums
fn impl_deserialize_as_untagged<'a>(variants: &'a [EnumVariant<'a>]) -> proc_macro2::TokenStream {
    let variants_deserialize_if = variants
        .iter()
        .map(gen_variant_match_if_exp)
        .collect::<Vec<proc_macro2::TokenStream>>();
    let generated_variants_from_type_path = variants.iter().map(|var| {
        let ty = var.field.ty.clone();
        quote! {#ty::VARIANTS}
    });
    quote! {
        use strum::VariantNames as _;
        #(#variants_deserialize_if)*
        let __known_variants: Vec<&str> = [
            #(#generated_variants_from_type_path),*
        ].concat().to_vec();

        Err(open_dds::traits::OpenDdDeserializeError {
            error: serde::de::Error::custom(format!(
                "unexpected value: `{}` expecting {}",
                    __tag_value_str,
                __known_variants.join(", ")
            )),
            path: jsonpath::JSONPath::new_key("kind"),
        })
    }
}

/// Generate if conditional code to match the variant
fn gen_variant_match_if_exp<'a>(variant: &'a EnumVariant<'a>) -> proc_macro2::TokenStream {
    let field = &variant.field;
    let variant_name = &variant.name;
    let ty = &field.ty.clone();
    quote! {
        if #ty::VARIANTS.contains(&__tag_value_str) {
            // The `kind` is removed to fetch __tag_value_str
            // Inserting it again to fecilitate deserialize into internal enums
            __object_map.insert("kind".to_string(), serde_json::json!(__tag_value_str));
            return Ok(Self::#variant_name(
                open_dds::traits::OpenDd::deserialize(serde_json::Value::Object(__object_map), path)?,
            ));
        }
    }
}

/// Generate the implementation for tagged enums
fn impl_deserialize_as_tagged(
    variants: &[EnumVariant<'_>],
    tag_type: &EnumTagType,
) -> proc_macro2::TokenStream {
    let variant_names = variants
        .iter()
        .filter_map(|var| {
            if var.hidden {
                None
            } else {
                Some(var.renamed_variant.to_string())
            }
        })
        .collect::<Vec<String>>();
    let variants = generate_enum_variants(variants, tag_type);
    let unexpected_variant_error =
        unexpected_variant_error(tag_type.generate_tag(), variant_names.as_slice());
    quote! {
        match __tag_value_str {
            #variants
            __ver => #unexpected_variant_error,
        }
    }
}

/// Generate match expression for each variant
fn generate_enum_variants(
    variants: &[EnumVariant<'_>],
    tag_type: &EnumTagType,
) -> proc_macro2::TokenStream {
    let deserialize_from = gen_deserialize_from(tag_type);
    let variants: Vec<proc_macro2::TokenStream> = variants.iter().map(|variant| {
        let variant_name = &variant.name;
        let variant_name_string = variant.renamed_variant.to_string();
        let variant_name_str = variant_name_string.as_str();
        let parsed_variant = match tag_type {
            EnumTagType::Internal {..} | EnumTagType::External => quote! {
                open_dds::traits::OpenDd::deserialize(#deserialize_from, path)?
            },
            EnumTagType::Adjacent {tag:_, content} => quote! {
                open_dds::traits::deserialize_key(#deserialize_from, path, #content.to_string())?
            },
        };

        if let Some(alias) = &variant.alias {
            let alias_str = alias.as_str();
            quote! {
                #variant_name_str | #alias_str => {
                        Ok(Self::#variant_name(#parsed_variant))
                }
            }
        } else {
            quote! {
                #variant_name_str => {
                        Ok(Self::#variant_name(#parsed_variant))
                }
            }
        }
    }).collect();

    quote! {
        #(#variants),*
    }
}

fn gen_deserialize_from(tag_type: &EnumTagType) -> proc_macro2::TokenStream {
    match tag_type {
        EnumTagType::Internal { .. } | EnumTagType::External { .. } => quote! {
            serde_json::Value::Object(__object_map)
        },
        EnumTagType::Adjacent { tag: _, content } => quote! {
            __object_map.remove(#content).ok_or_else(|| open_dds::traits::OpenDdDeserializeError {
                error: serde::de::Error::missing_field(#content),
                path: jsonpath::JSONPath::new(),
            })?
        },
    }
}

fn unexpected_variant_error(
    tag: Option<String>,
    known_variants: &[String],
) -> proc_macro2::TokenStream {
    let known_variants = known_variants.join(", ");

    let path_exp = if let Some(tag) = tag {
        quote! { jsonpath::JSONPath::new_key(#tag) }
    } else {
        quote! { jsonpath::JSONPath::new() }
    };

    quote! {
        Err(open_dds::traits::OpenDdDeserializeError  {
            error: serde::de::Error::unknown_variant(__ver, &[#known_variants]),
            path: #path_exp,
        })
    }
}

fn impl_json_schema_untagged(variants: &[EnumVariant<'_>]) -> proc_macro2::TokenStream {
    let schemas = variants
        .iter()
        .filter_map(|variant| {
            if variant.hidden {
                return None;
            }
            let ty = &variant.field.ty.clone();
            Some(quote! {
                open_dds::traits::gen_subschema_for::<#ty>(gen)
            })
        })
        .collect::<Vec<_>>();
    helpers::variant_subschemas(false, &schemas)
}

fn impl_json_schema_tagged(
    variants: &[EnumVariant<'_>],
    tag_type: &EnumTagType,
) -> proc_macro2::TokenStream {
    match tag_type {
        EnumTagType::Internal { tag } => {
            let mut unique_names = std::collections::HashSet::new();
            let mut count = 0;
            let variant_schemas = variants
                .iter()
                .filter_map(|variant| {
                    if variant.hidden {
                        return None;
                    }

                    unique_names.insert(variant.renamed_variant.to_string());
                    count += 1;

                    let name = &variant.renamed_variant;
                    let ty = &variant.field.ty.clone();
                    Some(quote! {{
                        let mut schema = <#ty as open_dds::traits::OpenDd>::json_schema(gen);

                        fn add_tag_to_json_schema(schema_internal: &mut schemars::schema::Schema) {
                            if let schemars::schema::Schema::Object(schemars::schema::SchemaObject {
                                object: Some(object_schema),
                                ..
                            }) = schema_internal
                            {
                                let inserted_index = object_schema.properties.len();
                                object_schema.properties.insert(
                                    #tag.to_string(),
                                    schemars::schema::Schema::Object(schemars::schema::SchemaObject {
                                        instance_type: Some(schemars::schema::InstanceType::String.into()),
                                        enum_values: Some(vec![#name.into()]),
                                        ..Default::default()
                                    }),
                                );
                                // Move the tag property to the front of the object, so that tooling displays
                                // it first.
                                object_schema.properties.move_index(inserted_index, 0);
                                object_schema.required.insert(#tag.into());
                            } else {
                                // Okay to panic since this is only used at compile time
                                panic!("Unexpected schema");
                            }
                        }

                        if let schemars::schema::Schema::Object(schemars::schema::SchemaObject {
                            subschemas: Some(subschema),
                            ..
                        }) = &mut schema
                        {
                            if let Some(ref mut one_ofs) = subschema.one_of {
                                for one_of in one_ofs.iter_mut() {
                                    add_tag_to_json_schema(one_of);
                                }
                            } else {
                                // Okay to panic since this is only used at compile time
                                panic!("Unexpected schema");
                            }
                        } else if let schemars::schema::Schema::Object(schemars::schema::SchemaObject {
                            object: Some(_), ..
                        }) = schema
                        {
                            add_tag_to_json_schema(&mut schema);
                        } else {
                            // Okay to panic since this is only used at compile time
                            panic!("Unexpected schema");
                        }
                        schema
                    }})

                })
                .collect::<Vec<_>>();

            helpers::variant_subschemas(unique_names.len() == count, &variant_schemas)
        }
        EnumTagType::Adjacent { tag, content } => {
            let mut unique_names = std::collections::HashSet::new();
            let mut count = 0;
            let variant_schemas = variants
                .iter()
                .filter_map(|variant| {
                    if variant.hidden {
                        return None;
                    }

                    unique_names.insert(variant.renamed_variant.to_string());
                    count += 1;

                    let name = &variant.renamed_variant;
                    let schema = helpers::schema_object(&quote! {
                        instance_type: Some(schemars::schema::InstanceType::String.into()),
                        enum_values: Some(vec![#name.into()]),
                    });
                    let ty = &variant.field.ty.clone();
                    let content_schema = quote! {
                        open_dds::traits::gen_subschema_for::<#ty>(gen)
                    };
                    let metadata_expr = build_variant_json_schema_metadata(
                        variant.doc_description.as_ref(),
                        &variant.json_schema_opts,
                    );
                    Some(helpers::schema_object(&quote! {
                        instance_type: Some(schemars::schema::InstanceType::Object.into()),
                        metadata: Some(Box::new(#metadata_expr)),
                        object: Some(Box::new(schemars::schema::ObjectValidation {
                            properties: {
                                let mut props = schemars::Map::new();
                                props.insert(#tag.to_owned(), #schema);
                                props.insert(#content.to_owned(), #content_schema);
                                props
                            },
                            required: {
                                let mut required = schemars::Set::new();
                                required.insert(#tag.to_owned());
                                required.insert(#content.to_owned());
                                required
                            },
                            additional_properties: Some(Box::new(false.into())),
                            ..Default::default()
                        })),
                    }))
                })
                .collect::<Vec<_>>();

            helpers::variant_subschemas(unique_names.len() == count, &variant_schemas)
        }
        EnumTagType::External => {
            let mut unique_names = std::collections::HashSet::new();
            let mut count = 0;
            let variant_schemas = variants
                .iter()
                .filter_map(|variant| {
                    if variant.hidden {
                        return None;
                    }

                    unique_names.insert(variant.renamed_variant.to_string());
                    count += 1;

                    let name = &variant.renamed_variant;
                    let ty = &variant.field.ty.clone();
                    let content_schema = quote! {
                        open_dds::traits::gen_subschema_for::<#ty>(gen)
                    };
                    let metadata_expr = build_variant_json_schema_metadata(
                        variant.doc_description.as_ref(),
                        &variant.json_schema_opts,
                    );
                    Some(helpers::schema_object(&quote! {
                        instance_type: Some(schemars::schema::InstanceType::Object.into()),
                        metadata: Some(Box::new(#metadata_expr)),
                        object: Some(Box::new(schemars::schema::ObjectValidation {
                            properties: {
                                let mut props = schemars::Map::new();
                                props.insert(#name.to_owned(), #content_schema);
                                props
                            },
                            required: {
                                let mut required = schemars::Set::new();
                                required.insert(#name.to_owned());
                                required
                            },
                            additional_properties: Some(Box::new(false.into())),
                            ..Default::default()
                        })),
                    }))
                })
                .collect::<Vec<_>>();

            helpers::variant_subschemas(unique_names.len() == count, &variant_schemas)
        }
    }
}

fn build_variant_json_schema_metadata(
    doc_description: Option<&String>,
    json_schema_opts: &JsonSchemaVariantOpts,
) -> proc_macro2::TokenStream {
    let description_expr = doc_description
        .map(|description| {
            quote! {
                metadata.description = Some(#description.to_string());
            }
        })
        .unwrap_or_default();
    let title_expr = json_schema_opts
        .title
        .as_ref()
        .map(|title| {
            quote! {
                metadata.title = Some(#title.to_string());
            }
        })
        .unwrap_or_default();
    let example_expr = helpers::set_metadata_examples(&json_schema_opts.examples);
    quote! {
        {
            let mut metadata = schemars::schema::Metadata::default();
            #title_expr
            #description_expr
            #example_expr
            metadata
        }
    }
}
