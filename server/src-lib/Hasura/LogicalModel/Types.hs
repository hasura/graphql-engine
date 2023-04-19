-- | A name for a logical model as it is recognized by the graphql schema.
module Hasura.LogicalModel.Types
  ( LogicalModelName (..),
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
import Data.HashMap.Strict.InsOrd qualified as InsOrd
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

-- | either a scalar type or a reference to another logical model
data LogicalModelField b
  = LogicalModelScalarField
      { lmfName :: Column b,
        lmfType :: ScalarType b, -- int, string, blah
        lmfNullable :: Bool,
        lmfDescription :: Maybe Text
      }
  | LogicalModelArrayReference
      { lmfName :: Column b,
        lmfLogicalModel :: LogicalModelName -- name of another logical model
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
        lmfName <- obj .: "name"
        pure (LogicalModelArrayReference {..})

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
  toJSON (LogicalModelArrayReference {..}) =
    object ["name" .= lmfName, "logical_model" .= lmfLogicalModel]

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
    (InsOrd.InsOrdHashMap (Column b) (LogicalModelField b))
    (InsOrd.InsOrdHashMap (Column b) (LogicalModelField b))
logicalModelFieldMapCodec =
  AC.dimapCodec
    ( InsOrd.fromList
        . fmap
          ( \lmf -> (lmfName lmf, lmf)
          )
    )
    ( fmap snd . InsOrd.toList
    )
    (AC.codec @[LogicalModelField b])
