use quote::quote;

use crate::container::*;
use crate::helpers;

pub fn impl_opendd_struct(name: &syn::Ident, data: &StructData<'_>) -> TraitImpls {
    let (impl_deserialize, json_schema_expr) = match &data {
        StructData::Newtype(field) => (
            quote! {
                let __internal = open_dds::traits::deserialize_index(json, path.clone(), 0)?;
                Ok(#name(__internal))
            },
            impl_json_schema_newtype(field),
        ),
        StructData::Named(named_fields) => (
            impl_deserialize_named_fields(name, named_fields),
            impl_json_schema_named_fields(named_fields),
        ),
    };

    TraitImpls {
        deserialize: impl_deserialize,
        json_schema: json_schema_expr,
    }
}

fn impl_json_schema_newtype(field: &syn::Field) -> proc_macro2::TokenStream {
    let ty = &field.ty.clone();
    quote! {
        open_dds::traits::gen_subschema_for::<#ty>(generator)
    }
}

fn impl_deserialize_named_fields<'a>(
    name: &'a syn::Ident,
    named_fields: &[NamedField<'a>],
) -> proc_macro2::TokenStream {
    let expected_fields = named_fields
        .iter()
        .map(|field| field.renamed_field.to_string())
        .collect::<Vec<String>>();
    let named_fields_value = generate_named_fields_value(name, named_fields);
    let unexpected_fields_error = if expected_fields.is_empty() {
        quote! {
            return Err(open_dds::traits::OpenDdDeserializeError {
                error: serde::de::Error::custom(format!(
                    "unexpected keys: {}; expecting empty object",
                    __remaining_keys.join(", "),
                )),
                path: jsonpath::JSONPath::new(),
            });
        }
    } else {
        quote! {
            return Err(open_dds::traits::OpenDdDeserializeError {
                error: serde::de::Error::custom(format!(
                    "unexpected keys: {}; expecting: {}",
                    __remaining_keys.join(", "),
                    [#(#expected_fields),*].join(", "),
                )),
                path: jsonpath::JSONPath::new(),
            });
        }
    };
    quote! {
        let mut __object_map = match json {
            serde_json::Value::Object(map) => map,
            _ => {
                return Err(open_dds::traits::OpenDdDeserializeError {
                    error: serde::de::Error::invalid_type(
                        serde::de::Unexpected::Other("not an object"),
                        &"object",
                    ),
                    path: jsonpath::JSONPath::new(),
                })
            },
        };
        let __value = #named_fields_value;
        let __remaining_keys = __object_map.keys().cloned().collect::<Vec<_>>();
        // Check for unexpected keys
        if !__remaining_keys.is_empty() {
            #unexpected_fields_error
        }
        Ok(__value)
    }
}

fn generate_named_fields_value<'a>(
    name: &'a syn::Ident,
    fields: &[NamedField<'a>],
) -> proc_macro2::TokenStream {
    let mut field_deserializations = Vec::new();
    for field in fields {
        let field_name = field.field_name;
        let field_name_str = field.renamed_field.as_str();

        let field_value_deserialize = quote! {
            |__value| open_dds::traits::deserialize_key(__value, path.clone(), #field_name_str.to_string())
        };

        let deserialize_field = quote! {
            __object_map.remove(#field_name_str).map(#field_value_deserialize)
        };

        let deserialize_alias = match &field.field_alias {
            None => quote! {},
            Some(alias) => quote! {
                .or_else(|| __object_map.remove(#alias).map(#field_value_deserialize))
            },
        };

        let field_value_fallback = if let Some(default) = &field.default {
            match default {
                DefaultAttribute::Flag => {
                    quote! {
                        .unwrap_or_default()
                    }
                }
                DefaultAttribute::Expr(default_exp) => {
                    quote! {
                        .unwrap_or(#default_exp)
                    }
                }
            }
        } else if field.is_optional {
            quote! {
                .unwrap_or(None)
            }
        } else {
            quote! {
                .ok_or_else(|| open_dds::traits::OpenDdDeserializeError {
                    error: serde::de::Error::missing_field(#field_name_str),
                    path: jsonpath::JSONPath::new(),
                })?
            }
        };

        field_deserializations.push(quote! {
            #field_name: #deserialize_field #deserialize_alias .transpose()? #field_value_fallback
        });
    }

    quote! {
        #name {
            #(#field_deserializations),*
        }
    }
}

fn impl_json_schema_named_fields(fields: &[NamedField<'_>]) -> proc_macro2::TokenStream {
    let mut fields_gen = Vec::new();
    let visible_fields = fields.iter().filter(|f| !f.hidden);
    for field in visible_fields {
        let field_name = field.renamed_field.as_str();
        let ty = field.field_type.clone();

        let default_prop = if let Some(default) = &field.default {
            let default_exp = field.default_exp.as_ref().map_or_else(
                || match default {
                    DefaultAttribute::Flag => {
                        quote! {
                            serde_json::json!(<#ty as Default>::default())
                        }
                    }
                    DefaultAttribute::Expr(default_exp) => {
                        quote! {
                            serde_json::json!(#default_exp)
                        }
                    }
                },
                quote::ToTokens::to_token_stream,
            );
            quote! {
                metadata.default = Some(#default_exp);
            }
        } else {
            proc_macro2::TokenStream::new()
        };

        let description = if let Some(d) = field.description.as_ref() {
            quote! {
                metadata.description = Some(#d.to_string());
            }
        } else {
            proc_macro2::TokenStream::new()
        };

        let title = if let Some(t) = &field.title {
            quote! {
                metadata.title = Some(#t.to_string());
            }
        } else {
            proc_macro2::TokenStream::new()
        };

        // We only modify the schema if necessary, as doing so adds extra levels
        // of nesting for the `$ref` value. (According to JSON Schema, `$ref`
        // cannot be mixed with other properties, and so `schemars` will nest
        // it inside an `allOf` field.)
        let schema_with_metadata = if default_prop.is_empty() && description.is_empty() {
            quote! {
                schema
            }
        } else {
            quote! {
                let mut schema_object = schema.into_object();
                let metadata = schema_object.metadata();
                #default_prop
                #description
                #title
                schemars::schema::Schema::Object(schema_object)
            }
        };

        let field_schema_exp = quote! {
            {
                let schema = open_dds::traits::gen_subschema_for::<#ty>(generator);
                #schema_with_metadata
            }
        };
        let properties_insert = quote! {
            properties.insert(
                #field_name.to_string(), #field_schema_exp
            );
        };
        let required_insert = if field.default.is_none() && !field.is_optional {
            quote! {
                required.insert(#field_name.to_string());
            }
        } else {
            proc_macro2::TokenStream::new()
        };
        fields_gen.push(quote! {
            {
                #properties_insert
                #required_insert
            }
        });
    }
    let init_vars = quote! {
        let mut properties = schemars::Map::new();
        let mut required = schemars::Set::new();
    };

    let schema_object = helpers::schema_object(&quote! {
        instance_type: Some(schemars::schema::InstanceType::Object.into()),
        object: Some(Box::new(schemars::schema::ObjectValidation{
            properties,
            additional_properties: Some(Box::new(false.into())),
            required,
            ..Default::default()
        })),
    });

    quote! {
        #init_vars
        #(#fields_gen)*
        #schema_object
    }
}
