{-# LANGUAGE UndecidableInstances #-}

module Hasura.LogicalModel.Metadata
  ( LogicalModelMetadata (..),
    LogicalModelName (..),
    WithLogicalModel (..),
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec)
import Autodocodec qualified as AC
import Data.Aeson (FromJSON (parseJSON), ToJSON, (.!=), (.:), (.:?))
import Data.Aeson qualified as J
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Hasura.LogicalModel.Types
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Common (SourceName, ToAesonPairs (toAesonPairs), defaultSource)
import Hasura.RQL.Types.Permission (SelPermDef, _pdRole)
import Hasura.RQL.Types.Roles (RoleName)

-- | Description of a logical model for use in metadata (before schema cache)
data LogicalModelMetadata (b :: BackendType) = LogicalModelMetadata
  { _lmmName :: LogicalModelName,
    _lmmFields :: LogicalModelFields b,
    _lmmDescription :: Maybe Text,
    _lmmSelectPermissions :: InsOrdHashMap RoleName (SelPermDef b)
  }
  deriving (Generic)

instance (Backend b) => HasCodec (LogicalModelMetadata b) where
  codec =
    AC.CommentCodec
      ("A return type.")
      $ AC.object (backendPrefix @b <> "LogicalModelMetadata")
      $ LogicalModelMetadata
      <$> AC.requiredField "name" nameDoc
      AC..= _lmmName
        <*> AC.requiredFieldWith "fields" logicalModelFieldMapCodec fieldsDoc
      AC..= _lmmFields
        <*> AC.optionalField "description" descriptionDoc
      AC..= _lmmDescription
        <*> optSortedList "select_permissions" _pdRole
      AC..= _lmmSelectPermissions
    where
      nameDoc = "A name for a logical model"
      fieldsDoc = "Return types for the logical model"
      descriptionDoc = "Optional description text which appears in the GraphQL Schema."

      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (LogicalModelMetadata b))
  instance
    (Backend b) => FromJSON (LogicalModelMetadata b)

deriving via
  (Autodocodec (LogicalModelMetadata b))
  instance
    (Backend b) => ToJSON (LogicalModelMetadata b)

deriving stock instance (Backend b) => Eq (LogicalModelMetadata b)

deriving stock instance (Backend b) => Show (LogicalModelMetadata b)

-- | A wrapper to tie something to a particular native query. Specifically, it
-- assumes the underlying '_wlmInfo' is represented as an object, and adds two
-- keys to that object: @source@ and @root_field_name@.
data WithLogicalModel a = WithLogicalModel
  { _wlmSource :: SourceName,
    _wlmName :: LogicalModelName,
    _wlmInfo :: a
  }
  deriving stock (Eq, Show)

-- | something to note here: if the `a` contains a `name` or `source` key then
-- this won't work anymore.
instance (FromJSON a) => FromJSON (WithLogicalModel a) where
  parseJSON = J.withObject "LogicalModel" \obj -> do
    _wlmSource <- obj .:? "source" .!= defaultSource
    _wlmName <- obj .: "name"
    _wlmInfo <- parseJSON (J.Object obj)

    pure WithLogicalModel {..}

instance (ToAesonPairs a) => ToJSON (WithLogicalModel a) where
  toJSON (WithLogicalModel source name info) =
    J.object $ ("source", J.toJSON source) : ("name", J.toJSON name) : toAesonPairs info
