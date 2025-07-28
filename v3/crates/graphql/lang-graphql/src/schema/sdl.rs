use super::build;
use super::*;

use crate::ast::common as ast;
use crate::ast::schema as sdl;
use crate::ast::spanning::Positioned;
use crate::parser;

#[derive(Debug, PartialEq, Clone)]
pub struct SDL {
    types: HashMap<ast::Name, sdl::TypeDefinition>,
    query: ast::Name,
    mutation: Option<ast::Name>,
    subscription: Option<ast::Name>,
}

impl SDL {
    pub fn new(schema: &str) -> std::result::Result<Self, SDLError> {
        let document = parser::Parser::new(schema)
            .parse_schema_document()
            .map_err(SDLError::ParseFailure)?;
        let mut type_definitions = HashMap::new();
        let mut schema_definition = None;
        for definition in document.definitions {
            match definition.item {
                sdl::TypeSystemDefinition::Schema(definition) => {
                    if schema_definition.replace(definition).is_some() {
                        return Err(SDLError::MultipleSchemaDefinitions);
                    }
                }
                sdl::TypeSystemDefinition::Type(type_definition) => {
                    let type_name = type_definition.name().item.clone();
                    if type_definitions
                        .insert(type_name.clone(), type_definition)
                        .is_some()
                    {
                        return Err(SDLError::DuplicateDefinitions(type_name));
                    }
                }
                sdl::TypeSystemDefinition::Directive(_) => {}
            }
        }
        let mut root_definitions = HashMap::new();
        if let Some(definition) = schema_definition {
            for operation_definition in &definition.operation_types {
                let (operation_type, type_name) = &operation_definition.item;
                root_definitions.insert(operation_type.item, type_name.item.clone());
            }
        }
        let query_root = {
            let type_name = root_definitions
                .remove(&ast::OperationType::Query)
                .unwrap_or_else(|| mk_name!("Query"));
            match type_definitions
                .get(&type_name)
                .ok_or_else(|| SDLError::TypeNotDefined(type_name.clone()))?
            {
                sdl::TypeDefinition::Object(definition) => definition.name.item.clone(),
                _ => return Err(SDLError::ExpectedObjectDefinition(type_name.clone())),
            }
        };
        let mutation_root = {
            let type_name = root_definitions
                .remove(&ast::OperationType::Mutation)
                .unwrap_or_else(|| mk_name!("Mutation"));
            match type_definitions.get(&type_name) {
                Some(sdl::TypeDefinition::Object(definition)) => Some(definition.name.item.clone()),
                Some(_) => return Err(SDLError::ExpectedObjectDefinition(type_name.clone())),
                None => None,
            }
        };
        let subscription_root = {
            let type_name = root_definitions
                .remove(&ast::OperationType::Subscription)
                .unwrap_or_else(|| mk_name!("Subscription"));
            match type_definitions.get(&type_name) {
                Some(sdl::TypeDefinition::Object(definition)) => Some(definition.name.item.clone()),
                Some(_) => return Err(SDLError::ExpectedObjectDefinition(type_name.clone())),
                None => None,
            }
        };
        Ok(SDL {
            types: type_definitions,
            query: query_root,
            mutation: mutation_root,
            subscription: subscription_root,
        })
    }

    pub fn build_schema(&self) -> std::result::Result<Schema<SDL>, SDLError> {
        build::build_schema(self)
    }
}

#[derive(Debug, Clone)]
pub enum SDLError {
    ParseFailure(Positioned<parser::Error>),
    Internal(build::Error),
    TypeNotDefined(ast::Name),
    DuplicateDefinitions(ast::Name),
    MultipleSchemaDefinitions,
    ExpectedObjectDefinition(ast::Name),
}

impl From<build::Error> for SDLError {
    fn from(value: build::Error) -> Self {
        Self::Internal(value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Namespace;

impl Display for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("NO_NAMESPACE")
    }
}

pub struct SDLNamespacedGetter();

impl NamespacedGetter<SDL> for SDLNamespacedGetter {
    fn get<'s, C>(
        &self,
        namespaced: &'s Namespaced<SDL, C>,
    ) -> Option<(&'s C, &'s <SDL as SchemaContext>::NamespacedNodeInfo)> {
        Some((&namespaced.data, &()))
    }
}

impl SchemaContext for SDL {
    type Namespace = Namespace;
    type GenericNodeInfo = ();
    type NamespacedNodeInfo = ();

    fn introspection_node() -> Self::GenericNodeInfo {}

    type TypeId = ast::Name;

    fn to_type_name(type_id: &Self::TypeId) -> ast::TypeName {
        ast::TypeName(type_id.clone())
    }

    type SchemaError = SDLError;

    fn build_type_info(
        &self,
        builder: &mut Builder<Self>,
        type_id: &Self::TypeId,
    ) -> std::result::Result<TypeInfo<Self>, SDLError> {
        let definition = self
            .types
            .get(type_id)
            .ok_or_else(|| SDLError::TypeNotDefined(type_id.clone()))?;
        Ok(build::convert_type_definition(
            builder,
            |builder_in_closure, type_name| match type_name.as_str() {
                "String" => RegisteredTypeName::string(),
                "Int" => RegisteredTypeName::int(),
                "Float" => RegisteredTypeName::float(),
                "Boolean" => RegisteredTypeName::boolean(),
                "ID" => RegisteredTypeName::id(),
                _ => builder_in_closure.register_type(type_name.0),
            },
            definition,
        ))
    }

    fn get_schema_entry_point(&self) -> EntryPoint<Self> {
        EntryPoint {
            query: self.query.clone(),
            mutation: self.mutation.clone(),
            subscription: self.subscription.clone(),
        }
    }
}
