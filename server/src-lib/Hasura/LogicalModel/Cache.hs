{-# LANGUAGE TemplateHaskell #-}

-- | The representation of logical models as derived from the schema cache.
module Hasura.LogicalModel.Cache
  ( LogicalModelInfo (..),
    LogicalModelCache,
    lmiRootFieldName,
    lmiCode,
    lmiReturns,
    lmiArguments,
    lmiDescription,
  )
where

import Autodocodec (Autodocodec, HasCodec (codec))
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Hasura.CustomReturnType (CustomReturnType)
import Hasura.LogicalModel.Metadata (InterpolatedQuery, LogicalModelArgumentName, LogicalModelName)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, ScalarType)
import Hasura.SQL.Backend (BackendType)

type LogicalModelCache b = HashMap LogicalModelName (LogicalModelInfo b)

-- | The type into which 'LogicalModelMetadata' is resolved in
-- 'Hasura/RQL/DDL/Schema/Cache.buildSchemaCacheRule'.
data LogicalModelInfo (b :: BackendType) = LogicalModelInfo
  { _lmiRootFieldName :: LogicalModelName,
    _lmiCode :: InterpolatedQuery LogicalModelArgumentName,
    _lmiReturns :: CustomReturnType b,
    _lmiArguments :: HashMap LogicalModelArgumentName (ScalarType b),
    _lmiDescription :: Maybe Text
  }
  deriving stock (Generic)

instance (Backend b) => HasCodec (LogicalModelInfo b) where
  codec =
    AC.CommentCodec
      ("A query in expressed in native code (SQL) to add to the GraphQL schema with configuration.")
      $ AC.object (codecNamePrefix @b <> "LogicalModelMetadata")
      $ LogicalModelInfo
        <$> AC.requiredField "root_field_name" fieldNameDoc
          AC..= _lmiRootFieldName
        <*> AC.requiredField "code" sqlDoc
          AC..= _lmiCode
        <*> AC.requiredField "returns" returnsDoc
          AC..= _lmiReturns
        <*> AC.optionalFieldWithDefault "arguments" mempty argumentDoc
          AC..= _lmiArguments
        <*> AC.optionalField "description" descriptionDoc
          AC..= _lmiDescription
    where
      fieldNameDoc = "Root field name for the logical model"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the logical model which appears in the graphql schema"

deriving via
  Autodocodec (LogicalModelInfo b)
  instance
    Backend b => ToJSON (LogicalModelInfo b)

deriving via
  Autodocodec (LogicalModelInfo b)
  instance
    Backend b => FromJSON (LogicalModelInfo b)

makeLenses ''LogicalModelInfo
