use super::container::JsonSchemaMetadata;
use quote::quote;
use syn::{Attribute, Lit::Str, Meta::NameValue, MetaNameValue};

pub fn get_title_and_desc_from_doc(attrs: &[Attribute]) -> (Option<String>, Option<String>) {
    let doc = match get_doc(attrs) {
        None => return (None, None),
        Some(doc) => doc,
    };

    if doc.starts_with('#') {
        let mut split = doc.splitn(2, '\n');
        let title = split
            .next()
            .unwrap()
            .trim_start_matches('#')
            .trim()
            .to_owned();
        let maybe_desc = split.next().and_then(merge_description_lines);
        (none_if_empty(title), maybe_desc)
    } else {
        (None, merge_description_lines(&doc))
    }
}

fn merge_description_lines(doc: &str) -> Option<String> {
    let desc = doc
        .trim()
        .split("\n\n")
        .filter_map(|line| none_if_empty(line.trim().replace('\n', " ")))
        .collect::<Vec<_>>()
        .join("\n\n");
    none_if_empty(desc)
}

fn get_doc(attrs: &[Attribute]) -> Option<String> {
    let attrs = attrs
        .iter()
        .filter_map(|attr| {
            if !attr.path.is_ident("doc") {
                return None;
            }

            let meta = attr.parse_meta().ok()?;
            if let NameValue(MetaNameValue { lit: Str(s), .. }) = meta {
                return Some(s.value());
            }

            None
        })
        .collect::<Vec<_>>();

    let mut lines = attrs
        .iter()
        .flat_map(|a| a.split('\n'))
        .map(str::trim)
        .skip_while(|s| s.is_empty())
        .collect::<Vec<_>>();

    if let Some(&"") = lines.last() {
        lines.pop();
    }

    // Added for backward-compatibility, but perhaps we shouldn't do this
    // https://github.com/rust-lang/rust/issues/32088
    if lines.iter().all(|l| l.starts_with('*')) {
        for line in lines.iter_mut() {
            *line = line[1..].trim()
        }
        while let Some(&"") = lines.first() {
            lines.remove(0);
        }
    };

    none_if_empty(lines.join("\n"))
}

fn none_if_empty(s: String) -> Option<String> {
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

pub fn apply_schema_metadata(
    schema_expr: &proc_macro2::TokenStream,
    json_schema_metadata: JsonSchemaMetadata,
) -> proc_macro2::TokenStream {
    let title = json_schema_metadata.title;
    let description = json_schema_metadata
        .description
        .as_ref()
        .map(|s| quote! {description: Some(#s.to_owned()),})
        .unwrap_or_default();

    let examples = json_schema_metadata
        .example
        .as_ref()
        .map(|eg| {
            quote! {
                examples: [#eg()].to_vec(),
            }
        })
        .unwrap_or_default();
    quote! {
        let schema = {
            #schema_expr
        };
        schemars::_private::apply_metadata(schema, schemars::schema::Metadata {
            title: Some(#title.to_owned()),
            id: Some(format!("https://hasura.io/jsonschemas/metadata/{}", Self::_schema_name())),
            #description
            #examples
            ..Default::default()
        })
    }
}

pub fn schema_object(properties: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! {
        schemars::schema::Schema::Object(
            schemars::schema::SchemaObject {
                #properties
                ..Default::default()
            })
    }
}

/// Callers must determine if all subschemas are mutually exclusive. This can
/// be done for most tagging regimes by checking that all tag names are unique.
pub fn variant_subschemas(
    unique: bool,
    schemas: &[proc_macro2::TokenStream],
) -> proc_macro2::TokenStream {
    if unique {
        schema_object(&quote! {
            subschemas: Some(Box::new(schemars::schema::SubschemaValidation {
                one_of: Some(vec![#(#schemas),*]),
                ..Default::default()
            })),
        })
    } else {
        schema_object(&quote! {
            subschemas: Some(Box::new(schemars::schema::SubschemaValidation {
                any_of: Some(vec![#(#schemas),*]),
                ..Default::default()
            })),
        })
    }
}
