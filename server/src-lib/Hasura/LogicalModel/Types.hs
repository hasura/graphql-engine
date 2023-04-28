-- | A name for a logical model as it is recognized by the graphql schema.
module Hasura.LogicalModel.Types
  ( LogicalModelName (..),
    LogicalModelReferenceType (..),
    LogicalModelField (..),
    logicalModelFieldMapCodec,
  )
where

import Autodocodec
  ( HasCodec (codec),
    dimapCodec,
  )
import Autodocodec qualified as AC
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended (ToTxt)
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- The name of a logical model. This appears as a root field name in the graphql schema.
newtype LogicalModelName = LogicalModelName {getLogicalModelName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Data, Generic, Lift)

instance HasCodec LogicalModelName where
  codec = dimapCodec LogicalModelName getLogicalModelName codec

instance FromJSONKey LogicalModelName

instance ToJSONKey LogicalModelName

----

-- | Type of reference for another Logical Model - object or array
data LogicalModelReferenceType
  = ArrayReference
  | ObjectReference
  deriving stock (Eq, Ord, Generic, Show)
  deriving (FromJSON, ToJSON) via (AC.Autodocodec LogicalModelReferenceType)

instance HasCodec LogicalModelReferenceType where
  codec =
    AC.CommentCodec
      ("Link type of reference, it refers to a single object or an array of objects")
      $ AC.stringConstCodec
      $ (ArrayReference, "array") :| [(ObjectReference, "object")]

instance Hashable LogicalModelReferenceType

instance NFData LogicalModelReferenceType

----

-- | either a scalar type or a reference to another logical model
data LogicalModelField b
  = LogicalModelScalarField
      { lmfName :: Column b,
        lmfType :: ScalarType b, -- int, string, blah
        lmfNullable :: Bool,
        lmfDescription :: Maybe Text
      }
  | LogicalModelReference
      { lmfName :: Column b,
        lmfLogicalModel :: LogicalModelName, -- name of another logical model
        lmfReferenceType :: LogicalModelReferenceType
      }
  deriving (Generic)

instance (Backend b) => HasCodec (LogicalModelField b) where
  codec =
    AC.CommentCodec
      ("A scalar type or reference to another logical model")
      $ placeholderCodecViaJSON

instance (Backend b) => FromJSON (LogicalModelField b) where
  parseJSON = withObject "LogicalModelField" \o ->
    parseReference o <|> parseScalar o
    where
      parseScalar obj = do
        lmfName <- obj .: "name"
        lmfType <- obj .: "type"
        lmfNullable <- obj .:? "nullable" .!= False
        lmfDescription <- obj .:? "description"
        pure (LogicalModelScalarField {..})
      parseReference obj = do
        lmfLogicalModel <- obj .: "logical_model"
        lmfReferenceType <- obj .: "link_type"
        lmfName <- obj .: "name"
        pure (LogicalModelReference {..})

instance (Backend b) => ToJSON (LogicalModelField b) where
  toJSON (LogicalModelScalarField {..}) =
    object $
      [ "name" .= lmfName,
        "type" .= lmfType,
        "nullable" .= lmfNullable
      ]
        <> maybeDescription
    where
      maybeDescription = case lmfDescription of
        Just desc -> ["description" .= desc]
        Nothing -> []
  toJSON (LogicalModelReference {..}) =
    object ["name" .= lmfName, "logical_model" .= lmfLogicalModel, "link_type" .= lmfReferenceType]

deriving stock instance (Backend b) => Eq (LogicalModelField b)

deriving stock instance (Backend b) => Show (LogicalModelField b)

instance (Backend b) => Hashable (LogicalModelField b)

instance (Backend b) => NFData (LogicalModelField b)

-- we parse in as an array of NullableScalarTypeFromArray and then turn into
-- InsOrdHashMap because JSON objects cannot be depended on for ordering
logicalModelFieldMapCodec ::
  forall b.
  (Backend b) =>
  AC.Codec
    Value
    (InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b))
    (InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b))
logicalModelFieldMapCodec =
  AC.dimapCodec
    ( InsOrdHashMap.fromList
        . fmap
          ( \lmf -> (lmfName lmf, lmf)
          )
    )
    ( fmap snd . InsOrdHashMap.toList
    )
    (AC.codec @[LogicalModelField b])
