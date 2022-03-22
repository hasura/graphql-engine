-- | Provides utilities to convert a runtime schema back to a static
-- intrsospection schema.
module Hasura.GraphQL.Parser.Schema.Convert
  ( convertToSchemaIntrospection,
  )
where

import Hasura.GraphQL.Parser.Schema
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------

-- | Convert back from our "live" schema representation into a flat
-- static set of definitions.
convertToSchemaIntrospection :: Schema -> G.SchemaIntrospection
convertToSchemaIntrospection = G.SchemaIntrospection . fmap convertType . sTypes

-------------------------------------------------------------------------------

convertType :: SomeDefinitionTypeInfo -> G.TypeDefinition [G.Name] G.InputValueDefinition
convertType (SomeDefinitionTypeInfo Definition {..}) = case dInfo of
  TIScalar ->
    G.TypeDefinitionScalar $
      G.ScalarTypeDefinition
        { G._stdDescription = dDescription,
          G._stdName = dName,
          G._stdDirectives = noDirectives
        }
  TIEnum enumInfo ->
    G.TypeDefinitionEnum $
      G.EnumTypeDefinition
        { G._etdDescription = dDescription,
          G._etdName = dName,
          G._etdDirectives = noDirectives,
          G._etdValueDefinitions = map convertEnumValue $ toList enumInfo
        }
  TIInputObject (InputObjectInfo values) ->
    G.TypeDefinitionInputObject $
      G.InputObjectTypeDefinition
        { G._iotdDescription = dDescription,
          G._iotdName = dName,
          G._iotdDirectives = noDirectives,
          G._iotdValueDefinitions = map convertInputField values
        }
  TIObject (ObjectInfo fields interfaces) ->
    G.TypeDefinitionObject $
      G.ObjectTypeDefinition
        { G._otdDescription = dDescription,
          G._otdName = dName,
          G._otdDirectives = noDirectives,
          G._otdImplementsInterfaces = map getDefinitionName interfaces,
          G._otdFieldsDefinition = map convertField fields
        }
  TIInterface (InterfaceInfo fields possibleTypes) ->
    G.TypeDefinitionInterface $
      G.InterfaceTypeDefinition
        { G._itdDescription = dDescription,
          G._itdName = dName,
          G._itdDirectives = noDirectives,
          G._itdFieldsDefinition = map convertField fields,
          G._itdPossibleTypes = map getDefinitionName possibleTypes
        }
  TIUnion (UnionInfo possibleTypes) ->
    G.TypeDefinitionUnion $
      G.UnionTypeDefinition
        { G._utdDescription = dDescription,
          G._utdName = dName,
          G._utdDirectives = noDirectives,
          G._utdMemberTypes = map getDefinitionName possibleTypes
        }

convertEnumValue :: Definition EnumValueInfo -> G.EnumValueDefinition
convertEnumValue Definition {..} =
  G.EnumValueDefinition
    { G._evdDescription = dDescription,
      G._evdName = G.EnumValue dName,
      G._evdDirectives = noDirectives
    }

convertInputField :: Definition InputFieldInfo -> G.InputValueDefinition
convertInputField Definition {..} = case dInfo of
  InputFieldInfo typeInfo defaultValue ->
    G.InputValueDefinition
      { G._ivdDescription = dDescription,
        G._ivdName = dName,
        G._ivdType = toGraphQLType typeInfo,
        G._ivdDefaultValue = defaultValue,
        G._ivdDirectives = noDirectives
      }

convertField :: Definition FieldInfo -> G.FieldDefinition G.InputValueDefinition
convertField Definition {..} = case dInfo of
  FieldInfo arguments typeInfo ->
    G.FieldDefinition
      { G._fldDescription = dDescription,
        G._fldName = dName,
        G._fldArgumentsDefinition = map convertInputField arguments,
        G._fldType = toGraphQLType typeInfo,
        G._fldDirectives = noDirectives
      }

-------------------------------------------------------------------------------

getDefinitionName :: Definition a -> G.Name
getDefinitionName = dName

noDirectives :: [G.Directive Void]
noDirectives = []
