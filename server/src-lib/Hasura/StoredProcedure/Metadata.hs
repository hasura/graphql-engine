{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Metadata representation of a stored procedure in the metadata,
--   as well as a parser and prettyprinter for the query code.
module Hasura.StoredProcedure.Metadata
  ( StoredProcedureName (..),
    StoredProcedureMetadata (..),
    spmArguments,
    spmCode,
    spmDescription,
    spmReturns,
    spmArrayRelationships,
    spmRootFieldName,
    ArgumentName (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    module Hasura.StoredProcedure.Types,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Data.Text.Extended qualified as T
import Hasura.LogicalModel.Types
import Hasura.NativeQuery.InterpolatedQuery
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelDef (..), RelManualConfig (..))
import Hasura.StoredProcedure.Types (NullableScalarType (..), StoredProcedureName (..), nullableScalarTypeMapCodec)

-- | copy pasta'd from Hasura.RQL.Types.Metadata.Common, forgive me Padre i did
-- not have the heart for the Real Fix.
type Relationships = InsOrdHashMap RelName

---------------------------------------

-- | The representation of native queries within the metadata structure.
data StoredProcedureMetadata (b :: BackendType) = StoredProcedureMetadata
  { _spmRootFieldName :: StoredProcedureName,
    _spmCode :: InterpolatedQuery ArgumentName,
    _spmReturns :: LogicalModelName,
    _spmArguments :: HashMap ArgumentName (NullableScalarType b),
    _spmArrayRelationships :: Relationships (RelDef (RelManualConfig b)),
    _spmDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Eq (StoredProcedureMetadata b)

deriving instance Backend b => Show (StoredProcedureMetadata b)

instance (Backend b) => HasCodec (StoredProcedureMetadata b) where
  codec =
    CommentCodec
      ("A stored procedure as represented in metadata.")
      $ AC.object (backendPrefix @b <> "StoredProcedureMetadata")
      $ StoredProcedureMetadata
        <$> requiredField "root_field_name" fieldNameDoc
          AC..= _spmRootFieldName
        <*> requiredField "code" sqlDoc
          AC..= _spmCode
        <*> requiredField "returns" returnsDoc
          AC..= _spmReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
          AC..= _spmArguments
        <*> optSortedList "array_relationships" _rdName
          AC..= _spmArrayRelationships
        <*> optionalField "description" descriptionDoc
          AC..= _spmDescription
    where
      fieldNameDoc = "Root field name for the stored procedure"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the stored procedure which appears in the graphql schema"

      optSortedList ::
        (HasCodec a, Eq a, Hashable k, Ord k, T.ToTxt k) =>
        Text ->
        (a -> k) ->
        ObjectCodec (InsOrdHashMap k a) (InsOrdHashMap k a)
      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (StoredProcedureMetadata b))
  instance
    (Backend b) => (FromJSON (StoredProcedureMetadata b))

deriving via
  (Autodocodec (StoredProcedureMetadata b))
  instance
    (Backend b) => (ToJSON (StoredProcedureMetadata b))

makeLenses ''StoredProcedureMetadata
