{-# LANGUAGE UndecidableInstances #-}

module Hasura.LogicalModelResolver.Metadata
  ( InlineLogicalModelMetadata (..),
    LogicalModelIdentifier (..),
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec)
import Autodocodec qualified as AC
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Hasura.LogicalModel.Types
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Permission (SelPermDef, _pdRole)
import Hasura.RQL.Types.Roles (RoleName)

-- | the name of a Logical Model, or an inline Logical Model
data LogicalModelIdentifier (b :: BackendType)
  = LMILogicalModelName LogicalModelName
  | LMIInlineLogicalModel (InlineLogicalModelMetadata b)
  deriving (Generic)

deriving stock instance (Backend b) => Eq (LogicalModelIdentifier b)

deriving stock instance (Backend b) => Show (LogicalModelIdentifier b)

-- | forgive me, I really did try and do this the native Autodocodec way
-- and everything I did kept freezing the whole of HGE
instance (Backend b) => HasCodec (LogicalModelIdentifier b) where
  codec =
    AC.CommentCodec
      ("A name or definition of a Logical Model")
      $ placeholderCodecViaJSON

instance (Backend b) => FromJSON (LogicalModelIdentifier b) where
  parseJSON j =
    (LMILogicalModelName <$> parseJSON j)
      <|> (LMIInlineLogicalModel <$> parseJSON j)

instance (Backend b) => ToJSON (LogicalModelIdentifier b) where
  toJSON (LMILogicalModelName t) = toJSON t
  toJSON (LMIInlineLogicalModel t) = toJSON t

-- | Description of an inline logical model to use in metadata (before schema cache)
-- this has no name - it is up to the resolving user (ie, the Native Query,
-- etc) to give the generated type a name
data InlineLogicalModelMetadata (b :: BackendType) = InlineLogicalModelMetadata
  { _ilmmFields :: InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b),
    _ilmmSelectPermissions :: InsOrdHashMap RoleName (SelPermDef b)
  }
  deriving (Generic)

instance (Backend b) => HasCodec (InlineLogicalModelMetadata b) where
  codec =
    AC.CommentCodec
      ("A return type.")
      $ AC.object (backendPrefix @b <> "InlineLogicalModelMetadata")
      $ InlineLogicalModelMetadata
      <$> AC.requiredFieldWith "fields" logicalModelFieldMapCodec fieldsDoc
      AC..= _ilmmFields
        <*> optSortedList "select_permissions" _pdRole
      AC..= _ilmmSelectPermissions
    where
      fieldsDoc = "Return types for the logical model"

      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (InlineLogicalModelMetadata b))
  instance
    (Backend b) => FromJSON (InlineLogicalModelMetadata b)

deriving via
  (Autodocodec (InlineLogicalModelMetadata b))
  instance
    (Backend b) => ToJSON (InlineLogicalModelMetadata b)

deriving stock instance (Backend b) => Eq (InlineLogicalModelMetadata b)

deriving stock instance (Backend b) => Show (InlineLogicalModelMetadata b)
