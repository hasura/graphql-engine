//! Utilities for tidying JSON schema outputs.

mod reference_replacement;
mod schema_similarity;

use regex::Regex;
use schemars::schema;
use std::collections::BTreeMap;

/// Search a `schemars` `RootSchema` for duplicate definitions and remove duplicates if possible.
/// Duplicate entries follow a predictable naming scheme (`Foo`, `Foo2`, and so on).
pub fn deduplicate_definitions(schema: &mut schema::RootSchema) {
    let regex = Regex::new(r"^(.*?)\d+$").unwrap();
    let mut possible_duplicates = BTreeMap::new();

    for name in schema.definitions.keys() {
        for (_, [prefix]) in regex.captures_iter(name).map(|x| x.extract()) {
            // We can't mutate the map we're looping over, so we have to do this in two steps.
            // Note that this step is too liberal on its own: we will, for example, find
            // `MyStructV2` and mark it as a potential duplicate of `MyStructV`.
            possible_duplicates.insert(name.clone(), prefix.to_string());
        }
    }

    // Here, we resolve the problem with the last step: we look to see whether the "original" type
    // exists, and if it does, we check for schema equivalence. This rules out two things not
    // touched by the last step:
    //
    // * `MyStructV2` will be discarded from the list of potential duplicates if `MyStructV` isn't
    //   a type that exists in the schema.
    //
    // * If the second version of a type just ends up being called `MyStruct2` as a successor to
    //   `MyStruct`, we'll only deduplicate them if there was no semantic change.
    possible_duplicates.retain(|alias, original| {
        if let Some(check) = schema.definitions.get(alias) {
            if let Some(current) = schema.definitions.get(original) {
                return schema_similarity::schemas(current, check);
            }
        }

        false
    });

    for (alias, original) in possible_duplicates {
        // Remove the duplicated definition.
        schema.definitions.remove(&alias);

        // The `schemars` library places all definitions in the `definitions` namespace, so this is
        // safe as long as we're only using it via `schemars`. In the future, we may want to
        // generalise this.
        let find = format!("#/definitions/{alias}");
        let replace = format!("#/definitions/{original}");

        // Replace references in the top-level schema...
        reference_replacement::in_schema_object(&mut schema.schema, &find, &replace);

        // ... and all other attached definitions.
        for definition in schema.definitions.values_mut() {
            reference_replacement::in_schema(definition, &find, &replace);
        }
    }
}
