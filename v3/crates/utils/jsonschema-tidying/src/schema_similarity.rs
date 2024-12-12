//! Compare metadata-exclusive equality on schemas.

use schemars::schema::{
    ArrayValidation, Metadata, ObjectValidation, Schema, SchemaObject, SingleOrVec,
    SubschemaValidation,
};
use schemars::Map;

/// An equality comparison of two schemas that doesn't check the metadata's title or description.
/// See `metadatas`.
pub fn schemas(this: &Schema, that: &Schema) -> bool {
    match (this, that) {
        (Schema::Object(this), Schema::Object(that)) => schema_objects(this, that),
        _ => this == that,
    }
}

/// An equality comparison of two schema objects that doesn't check the metadata's title or
/// description. See `metadatas`.
pub fn schema_objects(this: &SchemaObject, that: &SchemaObject) -> bool {
    eq_option_with(
        this.metadata.as_deref(),
        that.metadata.as_deref(),
        metadatas,
    ) && this.instance_type == that.instance_type
        && this.format == that.format
        && this.enum_values == that.enum_values
        && this.const_value == that.const_value
        && eq_option_with(
            this.subschemas.as_deref(),
            that.subschemas.as_deref(),
            subschemas,
        )
        && this.number == that.number
        && this.string == that.string
        && eq_option_with(this.array.as_deref(), that.array.as_deref(), arrays)
        && eq_option_with(this.object.as_deref(), that.object.as_deref(), objects)
        && this.reference == that.reference
        && this.extensions == that.extensions
}

/// This function is the interesting part of this module. In short, `schemars` can end up creating
/// multiple definitions that are identical, but have different names (usually `Foo`, `Foo2`, and
/// so on). However, we can't just deduplicate naïvely - for example, some `Foo` type might have a
/// version 2 that is legitimately called `Foo2`, but the structures do not match. So, we need to
/// check our assumptions.
///
/// The reason this is so uncomfortable is because schemas are recursive structures, and many
/// branch points within the schema's tree are... schemas. So, it's non-trivial to do the equality
/// by other means (for example, setting metadata to `None` and doing regular equality): you're
/// still going to end up traversing the entire structure.
pub fn metadatas(this: &Metadata, that: &Metadata) -> bool {
    // We can check everything in the metadata for equality _except for_ the id and title. Given
    // that we hopefully won't be doing this deep equality very often (only when generating a JSON
    // schema and only when the types have suspiciously similar names), this isn't a particularly
    // hot code path.

    this.description == that.description
        && this.default == that.default
        && this.deprecated == that.deprecated
        && this.read_only == that.read_only
        && this.write_only == that.write_only
        && this.examples == that.examples
}

/// An equality comparison of two subschemas that doesn't check the metadata's title or
/// description. See `metadatas`.
pub fn subschemas(this: &SubschemaValidation, that: &SubschemaValidation) -> bool {
    eq_option_with(this.all_of.as_ref(), that.all_of.as_ref(), |x, y| {
        vec_of_schemas(x, y)
    }) && eq_option_with(this.any_of.as_ref(), that.any_of.as_ref(), |x, y| {
        vec_of_schemas(x, y)
    }) && eq_option_with(this.one_of.as_ref(), that.one_of.as_ref(), |x, y| {
        vec_of_schemas(x, y)
    }) && eq_option_with(this.not.as_deref(), that.not.as_deref(), schemas)
        && eq_option_with(
            this.if_schema.as_deref(),
            that.if_schema.as_deref(),
            schemas,
        )
        && eq_option_with(
            this.then_schema.as_deref(),
            that.then_schema.as_deref(),
            schemas,
        )
        && eq_option_with(
            this.else_schema.as_deref(),
            that.else_schema.as_deref(),
            schemas,
        )
}

/// An equality comparison of two vectors of schemas that doesn't check the metadata's title or
/// description. See `metadatas`.
pub fn vec_of_schemas(this: &[Schema], that: &[Schema]) -> bool {
    this.len() == that.len() && this.iter().zip(that.iter()).all(|(x, y)| schemas(x, y))
}

/// An equality comparison of two array validations that doesn't check the metadata's title or
/// description. See `metadatas`.
pub fn arrays(this: &ArrayValidation, that: &ArrayValidation) -> bool {
    eq_option_with(
        this.items.as_ref(),
        that.items.as_ref(),
        single_or_vec_schemas,
    ) && eq_option_with(
        this.additional_items.as_deref(),
        that.additional_items.as_deref(),
        schemas,
    ) && this.max_items == that.max_items
        && this.min_items == that.min_items
        && this.unique_items == that.unique_items
        && eq_option_with(this.contains.as_deref(), that.contains.as_deref(), schemas)
}

/// An equality comparison of two single-or-vectors-of schemas that doesn't check the metadata's
/// title or description. See `metadatas`.
pub fn single_or_vec_schemas(this: &SingleOrVec<Schema>, that: &SingleOrVec<Schema>) -> bool {
    match (this, that) {
        (SingleOrVec::Single(this), SingleOrVec::Single(that)) => schemas(this, that),
        (SingleOrVec::Vec(this), SingleOrVec::Vec(that)) => vec_of_schemas(this, that),
        _ => false,
    }
}

/// An equality comparison of two object validations that doesn't check the metadata's title or
/// description. See `metadatas`.
pub fn objects(this: &ObjectValidation, that: &ObjectValidation) -> bool {
    this.max_properties == that.max_properties
        && this.min_properties == that.min_properties
        && this.required == that.required
        && this.properties == that.properties
        && schema_maps(&this.pattern_properties, &that.pattern_properties)
        && eq_option_with(
            this.additional_properties.as_deref(),
            that.additional_properties.as_deref(),
            schemas,
        )
        && eq_option_with(
            this.property_names.as_deref(),
            that.property_names.as_deref(),
            schemas,
        )
}

/// An equality comparison of two maps of schemas that doesn't check the metadata's title or
/// description. See `metadatas`.
pub fn schema_maps(this: &Map<String, Schema>, that: &Map<String, Schema>) -> bool {
    if this.len() != that.len() {
        return false;
    }

    for (k, v) in this {
        if let Some(v_) = that.get(k) {
            if !schemas(v, v_) {
                return false;
            }
        }
    }

    true
}

/// A helper for comparing constrained equality on two `Option` values.
pub fn eq_option_with<T: PartialEq, F: FnOnce(&T, &T) -> bool>(
    this: Option<&T>,
    that: Option<&T>,
    with: F,
) -> bool {
    match (this, that) {
        (Some(x), Some(y)) => with(x, y),
        _ => this == that,
    }
}
