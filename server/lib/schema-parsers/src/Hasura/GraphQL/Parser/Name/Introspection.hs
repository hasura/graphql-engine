{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Hasura.GraphQL.Parser.Name.Introspection where

import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

-- * 4. Introspection

-- ** 4.1. Type Name Introspection

___typename :: G.Name
___typename = [G.name|__typename|]

-- ** 4.2. Schema Introspection

___Schema :: G.Name
___Schema = [G.name|__Schema|]

___schema :: G.Name
___schema = [G.name|__schema|]

___Type :: G.Name
___Type = [G.name|__Type|]

___type :: G.Name
___type = [G.name|__type|]

_name :: G.Name
_name = [G.name|name|]

-- *** First Class Documentation

_description :: G.Name
_description = [G.name|description|]

-- *** Deprecation

_isDeprecated :: G.Name
_isDeprecated = [G.name|isDeprecated|]

_deprecationReason :: G.Name
_deprecationReason = [G.name|deprecationReason|]

-- *** Schema Introspection Schema

___Directive :: G.Name
___Directive = [G.name|__Directive|]

___EnumValue :: G.Name
___EnumValue = [G.name|__EnumValue|]

___Field :: G.Name
___Field = [G.name|__Field|]

___InputValue :: G.Name
___InputValue = [G.name|__InputValue|]

___TypeKind :: G.Name
___TypeKind = [G.name|__TypeKind|]

_args :: G.Name
_args = [G.name|args|]

_defaultValue :: G.Name
_defaultValue = [G.name|defaultValue|]

_directives :: G.Name
_directives = [G.name|directives|]

_enumValues :: G.Name
_enumValues = [G.name|enumValues|]

_fields :: G.Name
_fields = [G.name|fields|]

_interfaces :: G.Name
_interfaces = [G.name|interfaces|]

_includeDeprecated :: G.Name
_includeDeprecated = [G.name|includeDeprecated|]

_inputFields :: G.Name
_inputFields = [G.name|inputFields|]

_isRepeatable :: G.Name
_isRepeatable = [G.name|isRepeatable|]

_locations :: G.Name
_locations = [G.name|locations|]

_kind :: G.Name
_kind = [G.name|kind|]

_ofType :: G.Name
_ofType = [G.name|ofType|]

_mutationType :: G.Name
_mutationType = [G.name|mutationType|]

_possibleTypes :: G.Name
_possibleTypes = [G.name|possibleTypes|]

_queryType :: G.Name
_queryType = [G.name|queryType|]

_subscriptionType :: G.Name
_subscriptionType = [G.name|subscriptionType|]

_type :: G.Name
_type = [G.name|type|]

_types :: G.Name
_types = [G.name|types|]

-- **** Type Kinds

_ENUM :: G.Name
_ENUM = [G.name|ENUM|]

_INPUT_OBJECT :: G.Name
_INPUT_OBJECT = [G.name|INPUT_OBJECT|]

_INTERFACE :: G.Name
_INTERFACE = [G.name|INTERFACE|]

_LIST :: G.Name
_LIST = [G.name|LIST|]

_NON_NULL :: G.Name
_NON_NULL = [G.name|NON_NULL|]

_OBJECT :: G.Name
_OBJECT = [G.name|OBJECT|]

_SCALAR :: G.Name
_SCALAR = [G.name|SCALAR|]

_UNION :: G.Name
_UNION = [G.name|UNION|]
