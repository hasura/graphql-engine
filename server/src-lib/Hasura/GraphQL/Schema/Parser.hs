{-# LANGUAGE PatternSynonyms #-}

-- | In 'Hasura.GraphQL.Parser', the 'Definition' type has a 'dOrigin' field
-- that allows to track where a fragment of GraphQL type information comes from.
-- This is useful for error reporting and internal repair mechanisms such as
-- inconsistency tracking.
--
-- Morally, within the HGE codebase, this origin is always 'MetadataObjId'.
-- However, in order to avoid an import of 'Hasura.RQL' from
-- 'Hasura.GraphQL.Parser', the 'dOrigin' has been defined through a type
-- parameter of 'Definition'.  This type parameter then has to get threaded
-- through throughout the 'Hasura.GraphQL.Parser' module hierarchy, so that it
-- ends up in a lot of types.  This is very noisy.
--
-- In order to avoid the noise of this type parameter, which really only has one
-- value, and is really only used in one type, this module erases the type
-- parameter by filling in the desired value, exporting type synonyms of the
-- now-fixed notion of "origin".  So most modules in the HGE codebase should
-- import this module rather than 'Hasura.GraphQL.Parser'.
module Hasura.GraphQL.Schema.Parser
  ( -- The pattern is as follows:
    -- 1. Export a type synonym which has the origin type parameter set to
    --    'MetadataObjId'
    FieldParser,
    -- 2. Export the constructor of the type.  Note that despite the use of
    --    'PatternSynonyms', there is no pattern being defined.  The reason for
    --    using 'PatternSynonyms' is that that extension (and the 'pattern'
    --    syntax) allows re-exporting a constructor of a type, without
    --    re-exporting its original associated type.  This is not possible in
    --    plain Haskell2010.
    pattern P.FieldParser,
    InputFieldsParser,
    pattern P.InputFieldsParser,
    Parser,
    pattern P.Parser,
    Schema,
    pattern P.Schema,
    Definition,
    pattern P.Definition,
    Type,
    Directive,
    pattern P.Directive,
    DirectiveInfo,
    pattern P.DirectiveInfo,
    FieldInfo,
    pattern P.FieldInfo,
    InputFieldInfo,
    pattern P.InputFieldInfo,
    HasTypeDefinitions,
    SomeDefinitionTypeInfo,
    pattern P.SomeDefinitionTypeInfo,
    TypeDefinitionsWrapper,
    pattern TypeDefinitionsWrapper,
    module Hasura.GraphQL.Parser,
  )
where

-- Re-export everything, except types whose type parameter we want to fill in in
-- this module.
import Hasura.GraphQL.Parser hiding
  ( Definition,
    Directive,
    DirectiveInfo,
    FieldInfo,
    FieldParser,
    HasTypeDefinitions,
    InputFieldInfo,
    InputFieldsParser,
    Parser,
    Schema,
    SomeDefinitionTypeInfo,
    Type,
    TypeDefinitionsWrapper,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.RQL.Types.Metadata.Object

type FieldParser = P.FieldParser MetadataObjId

type Parser = P.Parser MetadataObjId

type Schema = P.Schema MetadataObjId

type Type = P.Type MetadataObjId

type InputFieldsParser = P.InputFieldsParser MetadataObjId

type Definition = P.Definition MetadataObjId

type Directive = P.Directive MetadataObjId

type DirectiveInfo = P.DirectiveInfo MetadataObjId

type FieldInfo = P.FieldInfo MetadataObjId

type InputFieldInfo = P.InputFieldInfo MetadataObjId

type HasTypeDefinitions = P.HasTypeDefinitions MetadataObjId

type SomeDefinitionTypeInfo = P.SomeDefinitionTypeInfo MetadataObjId

type TypeDefinitionsWrapper = P.TypeDefinitionsWrapper MetadataObjId

-- | In order to aid type inference and type checking, we define this pattern
-- synonym (an actual one) which restricts 'P.TypeDefinitionsWrapper' to have
-- 'MetadataObjId' set for its origin type parameter.
pattern TypeDefinitionsWrapper :: () => forall a. HasTypeDefinitions a => a -> TypeDefinitionsWrapper
pattern TypeDefinitionsWrapper typeDef = P.TypeDefinitionsWrapper typeDef
