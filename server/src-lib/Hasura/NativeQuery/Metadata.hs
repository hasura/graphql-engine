{-# LANGUAGE UndecidableInstances #-}

-- | Metadata representation of a native query in the metadata,
--   as well as a parser and prettyprinter for the query code.
module Hasura.NativeQuery.Metadata
  ( NativeQueryName (..),
    NativeQueryMetadata (..),
    ArgumentName (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    module Hasura.NativeQuery.Types,
    WithNativeQuery (..),
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Data.Aeson (FromJSON (parseJSON), ToJSON, (.!=), (.:), (.:?))
import Data.Aeson qualified as J
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Data.Text.Extended qualified as T
import Hasura.LogicalModelResolver.Metadata (LogicalModelIdentifier)
import Hasura.NativeQuery.InterpolatedQuery
import Hasura.NativeQuery.Types (NativeQueryName (..), NullableScalarType (..))
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (RelName, SourceName, ToAesonPairs (toAesonPairs), defaultSource)
import Hasura.RQL.Types.Relationships.Local (RelDef (..), RelManualConfig (..))

-- | copy pasta'd from Hasura.RQL.Types.Metadata.Common, forgive me Padre i did
-- not have the heart for the Real Fix.
type Relationships = InsOrdHashMap RelName

---------------------------------------

-- | The representation of native queries within the metadata structure.
data NativeQueryMetadata (b :: BackendType) = NativeQueryMetadata
  { _nqmRootFieldName :: NativeQueryName,
    _nqmCode :: InterpolatedQuery ArgumentName,
    _nqmReturns :: LogicalModelIdentifier b,
    _nqmArguments :: HashMap ArgumentName (NullableScalarType b),
    _nqmArrayRelationships :: Relationships (RelDef (RelManualConfig b)),
    _nqmObjectRelationships :: Relationships (RelDef (RelManualConfig b)),
    _nqmDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (NativeQueryMetadata b)

deriving instance (Backend b) => Show (NativeQueryMetadata b)

instance (Backend b) => HasCodec (NativeQueryMetadata b) where
  codec =
    CommentCodec
      ("A native query as represented in metadata.")
      $ AC.object (backendPrefix @b <> "NativeQueryMetadata")
      $ NativeQueryMetadata
      <$> requiredField "root_field_name" fieldNameDoc
      AC..= _nqmRootFieldName
        <*> requiredField "code" sqlDoc
      AC..= _nqmCode
        <*> requiredField "returns" returnsDoc
      AC..= _nqmReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
      AC..= _nqmArguments
        <*> optSortedList "array_relationships" _rdName
      AC..= _nqmArrayRelationships
        <*> optSortedList "object_relationships" _rdName
      AC..= _nqmObjectRelationships
        <*> optionalField "description" descriptionDoc
      AC..= _nqmDescription
    where
      fieldNameDoc = "Root field name for the native query"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the native query which appears in the graphql schema"

      optSortedList ::
        (HasCodec a, Eq a, Hashable k, Ord k, T.ToTxt k) =>
        Text ->
        (a -> k) ->
        ObjectCodec (InsOrdHashMap k a) (InsOrdHashMap k a)
      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (NativeQueryMetadata b))
  instance
    (Backend b) => (FromJSON (NativeQueryMetadata b))

deriving via
  (Autodocodec (NativeQueryMetadata b))
  instance
    (Backend b) => (ToJSON (NativeQueryMetadata b))

-- | A wrapper to tie something to a particular native query. Specifically, it
-- assumes the underlying '_wlmInfo' is represented as an object, and adds two
-- keys to that object: @source@ and @root_field_name@.
data WithNativeQuery a = WithNativeQuery
  { _wnqSource :: SourceName,
    _wnqName :: NativeQueryName,
    _wnqInfo :: a
  }
  deriving stock (Eq, Show)

-- | something to note here: if the `a` contains a `name` or `source` key then
-- this won't work anymore.
instance (FromJSON a) => FromJSON (WithNativeQuery a) where
  parseJSON = J.withObject "NativeQuery" \obj -> do
    _wnqSource <- obj .:? "source" .!= defaultSource
    _wnqName <- obj .: "name"
    _wnqInfo <- J.parseJSON (J.Object obj)

    pure WithNativeQuery {..}

instance (ToAesonPairs a) => ToJSON (WithNativeQuery a) where
  toJSON (WithNativeQuery source name info) =
    J.object $ ("source", J.toJSON source) : ("name", J.toJSON name) : toAesonPairs info
