{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Metadata representation of a stored procedure in the metadata,
--   as well as a parser and prettyprinter for the query code.
module Hasura.StoredProcedure.Metadata
  ( StoredProcedureMetadata (..),
    spmStoredProcedure,
    spmConfig,
    spmArguments,
    spmDescription,
    spmReturns,
    ArgumentName (..),
    module Hasura.StoredProcedure.Types,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Hasura.LogicalModel.Types
import Hasura.LogicalModelResolver.Types (ArgumentName (..))
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.StoredProcedure.Types (NullableScalarType (..), StoredProcedureConfig (..), nullableScalarTypeMapCodec)

---------------------------------------

-- | The representation of stored procedures within the metadata structure.
data StoredProcedureMetadata (b :: BackendType) = StoredProcedureMetadata
  { _spmStoredProcedure :: FunctionName b,
    _spmConfig :: StoredProcedureConfig,
    _spmReturns :: LogicalModelName,
    _spmArguments :: HashMap ArgumentName (NullableScalarType b),
    _spmDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (StoredProcedureMetadata b)

deriving instance (Backend b) => Show (StoredProcedureMetadata b)

instance (Backend b) => HasCodec (StoredProcedureMetadata b) where
  codec =
    CommentCodec
      ("A stored procedure as represented in metadata.")
      $ AC.object (backendPrefix @b <> "StoredProcedureMetadata")
      $ StoredProcedureMetadata
      <$> AC.requiredField "stored_procedure" spDoc
      AC..= _spmStoredProcedure
        <*> requiredField "configuration" configDoc
      AC..= _spmConfig
        <*> requiredField "returns" returnsDoc
      AC..= _spmReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
      AC..= _spmArguments
        <*> optionalField "description" descriptionDoc
      AC..= _spmDescription
    where
      spDoc = "The name of the SQL stored procedure"
      configDoc = "The configuration for the SQL stored procedure"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the stored procedure which appears in the graphql schema"

deriving via
  (Autodocodec (StoredProcedureMetadata b))
  instance
    (Backend b) => (FromJSON (StoredProcedureMetadata b))

deriving via
  (Autodocodec (StoredProcedureMetadata b))
  instance
    (Backend b) => (ToJSON (StoredProcedureMetadata b))

makeLenses ''StoredProcedureMetadata
