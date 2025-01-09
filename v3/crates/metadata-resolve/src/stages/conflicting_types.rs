use crate::stages::boolean_expressions::BooleanExpressionTypeIdentifier;
use crate::types::warning::{ConflictingNameAcrossTypes, ConflictingSources, TypeSource, Warning};
use crate::Qualified;
use open_dds::types::CustomTypeName;
use std::collections::{BTreeMap, HashMap};

/// Struct to collect and track type names across different sources
struct CollectTypeNames<'a>(HashMap<&'a Qualified<CustomTypeName>, nonempty::NonEmpty<TypeSource>>);

impl<'a> CollectTypeNames<'a> {
    /// Creates a new instance of CollectTypeNames
    fn new() -> Self {
        CollectTypeNames(HashMap::new())
    }

    /// Inserts a new type name and its source into the collection
    /// If the name already exists, it adds the new source to the existing entry
    fn insert(&mut self, name: &'a Qualified<CustomTypeName>, source: TypeSource) {
        match self.0.entry(name) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                entry.get_mut().push(source);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(nonempty::NonEmpty::new(source));
            }
        }
    }

    /// Returns a list of warnings for conflicting names (names with more than one source)
    fn warn_conflicting_names(self) -> Vec<Warning> {
        self.0
            .into_iter()
            .filter_map(|(name, sources)| {
                if sources.len() > 1 {
                    Some(Warning::ConflictingNameAcrossTypes(
                        ConflictingNameAcrossTypes {
                            name: name.clone(),
                            conflicting_sources: ConflictingSources(sources),
                        },
                    ))
                } else {
                    None
                }
            })
            .collect()
    }
}

/// Checks for duplicate names across different type categories (scalar, object, boolean expression)
pub(crate) fn check_conflicting_names_across_types(
    scalar_types: &BTreeMap<
        Qualified<CustomTypeName>,
        super::scalar_type_representations::ScalarTypeRepresentation,
    >,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        super::object_relationships::ObjectTypeWithRelationships,
    >,
    boolean_expression_types: &super::boolean_expressions::BooleanExpressionTypes,
) -> Vec<Warning> {
    let mut all_type_names = CollectTypeNames::new();

    // Collect all scalar type names
    for name in scalar_types.keys() {
        all_type_names.insert(name, TypeSource::Scalar);
    }

    // Collect all object type names
    for name in object_types.keys() {
        all_type_names.insert(name, TypeSource::Object);
    }

    // Collect all boolean expression type names
    let all_boolean_expression_type_names = {
        let super::boolean_expressions::BooleanExpressionTypes {
            objects,
            scalars,
            object_aggregates,
            scalar_aggregates,
        } = boolean_expression_types;
        let mut names = objects.keys().collect::<Vec<_>>();

        // need to unwrap scalar names
        names.extend(
            scalars
                .keys()
                .filter_map(|bool_exp_identifier| match bool_exp_identifier {
                    BooleanExpressionTypeIdentifier::FromBooleanExpressionType(bool_exp_type) => {
                        Some(bool_exp_type)
                    }
                    BooleanExpressionTypeIdentifier::FromDataConnectorScalarRepresentation(_) => {
                        None
                    }
                })
                .collect::<Vec<_>>(),
        );
        names.extend(scalar_aggregates.keys());
        names.extend(object_aggregates.keys());
        names
    };
    for name in all_boolean_expression_type_names {
        all_type_names.insert(name, TypeSource::BooleanExpression);
    }

    // Return the list of conflicting names as warnings
    all_type_names.warn_conflicting_names()
}
