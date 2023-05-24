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
    InputObjectInfo,
    pattern P.InputObjectInfo,
    Parser,
    pattern P.Parser,
    Schema,
    pattern P.Schema,
    ConflictingDefinitions,
    pattern P.ConflictingDefinitions,
    Definition,
    pattern P.Definition,
    Type,
    TypeInfo,
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
    P.ParseErrorCode (..),
    toQErr,
    module Hasura.GraphQL.Parser,
    Memoize.MonadMemoize,
    memoizeOn,
    memoize,
  )
where

-- Re-export everything, except types whose type parameter we want to fill in in
-- this module.

import Control.Monad.Error.Class
import Control.Monad.Memoize qualified as Memoize
import Data.Typeable
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (ErrorMessage (fromErrorMessage))
import Hasura.GraphQL.Parser hiding
  ( ConflictingDefinitions (..),
    Definition,
    Directive,
    DirectiveInfo,
    FieldInfo,
    FieldParser,
    HasTypeDefinitions,
    InputFieldInfo,
    InputFieldsParser,
    InputObjectInfo,
    ParseErrorCode (..),
    Parser,
    Schema,
    SomeDefinitionTypeInfo,
    Type,
    TypeDefinitionsWrapper,
    TypeInfo,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Object
import Language.Haskell.TH qualified as TH

type FieldParser = P.FieldParser MetadataObjId

type Parser = P.Parser MetadataObjId

type Schema = P.Schema MetadataObjId

type ConflictingDefinitions = P.ConflictingDefinitions MetadataObjId

type Type = P.Type MetadataObjId

type TypeInfo = P.TypeInfo MetadataObjId

type InputFieldsParser = P.InputFieldsParser MetadataObjId

type InputObjectInfo = P.InputObjectInfo MetadataObjId

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
pattern TypeDefinitionsWrapper :: () => forall a. (HasTypeDefinitions a) => a -> TypeDefinitionsWrapper
pattern TypeDefinitionsWrapper typeDef = P.TypeDefinitionsWrapper typeDef

toQErr :: (MonadError QErr m) => Either ParseError a -> m a
toQErr = either (throwError . parseErrorToQErr) pure
  where
    parseErrorToQErr :: ParseError -> QErr
    parseErrorToQErr ParseError {pePath, peMessage, peCode} =
      (err400 (parseErrorCodeToCode peCode) (fromErrorMessage peMessage)) {qePath = pePath}

    parseErrorCodeToCode :: P.ParseErrorCode -> Code
    parseErrorCodeToCode P.ValidationFailed = ValidationFailed
    parseErrorCodeToCode P.ParseFailed = ParseFailed
    parseErrorCodeToCode P.ConflictingDefinitionsError = Unexpected
    parseErrorCodeToCode P.NotSupported = NotSupported

memoizeOn ::
  (Memoize.MonadMemoize m, Ord a, Typeable a, Typeable p, MonadParse n, Typeable b) =>
  -- | A unique name used to identify the function being memoized. There isn’t
  -- really any metaprogramming going on here, we just use a Template Haskell
  -- 'TH.Name' as a convenient source for a static, unique identifier.
  TH.Name ->
  -- | The value to use as the memoization key. It’s the caller’s
  -- responsibility to ensure multiple calls to the same function don’t use
  -- the same key.
  a ->
  -- | The value to be memoized. 'p' is intended to be either 'Parser k' or
  -- 'FieldParser'.
  m (p n b) ->
  m (p n b)
memoizeOn = Memoize.memoizeOn

-- | A wrapper around 'memoizeOn' that memoizes a function by using its argument
-- as the key.
memoize ::
  (Memoize.MonadMemoize m, Ord a, Typeable a, Typeable p, MonadParse n, Typeable b) =>
  TH.Name ->
  -- | A function generating something to be memoized. 'p' is intended to be
  -- either 'Parser k' or 'FieldParser'.
  (a -> m (p n b)) ->
  (a -> m (p n b))
memoize = Memoize.memoize
