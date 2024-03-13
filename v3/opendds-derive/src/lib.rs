use proc_macro::{self, TokenStream};
use quote::quote;
use syn::{self, parse_macro_input, DeriveInput};

mod container;
mod enum_derive;
mod helpers;
mod struct_derive;

use crate::container::*;

#[proc_macro_derive(OpenDd, attributes(opendd))]
pub fn derive(input_tok: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input_tok as DeriveInput);
    impl_opendd(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn impl_opendd(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let cont = Container::from_derive_input(&input)?;
    let TraitImpls {
        deserialize: impl_deserialize,
        json_schema,
    } = match cont.data {
        Data::Enum(EnumData::DefaultImpl) => {
            return Ok(quote! {
                open_dds::impl_OpenDd_default_for!(#name);
            })
        }
        Data::Enum(EnumData::Impl(impl_style, enum_variants)) => {
            enum_derive::impl_opendd_enum(impl_style, enum_variants)
        }
        Data::Struct(struct_data) => struct_derive::impl_opendd_struct(name, struct_data),
    };
    let json_schema_metadata = cont.json_schema_metadata;
    let schema_name = &json_schema_metadata.schema_name.to_string();
    let impl_json_schema = helpers::apply_schema_metadata(json_schema, json_schema_metadata);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    Ok(quote! {
        impl #impl_generics open_dds::traits::OpenDd for #name #ty_generics #where_clause {
            fn deserialize(json: serde_json::Value) -> Result<Self, open_dds::traits::OpenDdDeserializeError> {
                #impl_deserialize
            }

            fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {

                #impl_json_schema
            }

            fn _schema_name() -> String {
                #schema_name.to_owned()
            }

            fn _schema_is_referenceable() -> bool {
                true
            }
        }
    })
}
