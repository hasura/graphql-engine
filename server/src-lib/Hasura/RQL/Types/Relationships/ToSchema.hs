module Hasura.RQL.Types.Relationships.ToSchema
  ( ToSchemaRelationshipDef (..),
    FieldCall (..),
    RemoteArguments (..),
    RemoteFields (..),
    trrdRemoteField,
    trrdLhsFields,
    trrdRemoteSchema,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (prependFailure)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HM
import Data.Scientific
import Data.Text qualified as T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.RemoteSchema
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- metadata

-- | Metadata representation of a relationship to a remote schema.
--
-- FIXME: move this to Hasura/Metadata
data ToSchemaRelationshipDef = ToSchemaRelationshipDef
  { -- | Identifier for this mapping.
    _trrdRemoteSchema :: !RemoteSchemaName,
    -- | The lhs fields that must be forwarded to the remote schema.
    _trrdLhsFields :: !(HashSet FieldName),
    _trrdRemoteField :: !RemoteFields
  }
  deriving stock (Show, Eq, Generic)

instance NFData ToSchemaRelationshipDef

instance Cacheable ToSchemaRelationshipDef

-- | Targeted field in a remote schema relationship.
-- TODO: explain about subfields and why this is a container
newtype RemoteFields = RemoteFields {unRemoteFields :: NonEmpty FieldCall}
  deriving (Show, Eq, Generic)

instance NFData RemoteFields

instance Cacheable RemoteFields

instance FromJSON RemoteFields where
  parseJSON = prependFailure details . fmap RemoteFields . parseRemoteFields
    where
      details = "Remote fields are represented by an object that maps each field name to its arguments."
      parseRemoteFields = withObject "RemoteFields" \hashmap -> case HM.toList hashmap of
        [(fieldNameText, callValue)] -> do
          fieldName <- parseJSON (String fieldNameText)
          callObject <- parseJSON callValue
          arguments <- callObject .: "arguments"
          maybeSubField <- callObject .:? "field"
          subFields <-
            fromMaybe [] <$> for maybeSubField \fieldValue -> do
              remoteFields <- parseRemoteFields fieldValue
              pure (toList remoteFields)
          pure $ FieldCall {fcName = fieldName, fcArguments = arguments} :| subFields
        [] -> fail "Expecting one single mapping, received none."
        _ -> fail "Expecting one single mapping, received too many."

instance ToJSON RemoteFields where
  toJSON (RemoteFields fields) = remoteFieldsJson fields
    where
      remoteFieldsJson (field :| subfields) =
        object
          [ G.unName (fcName field)
              .= object
                ( catMaybes
                    [ Just $ "arguments" .= fcArguments field,
                      nonEmpty subfields <&> \sf -> "field" .= remoteFieldsJson sf
                    ]
                )
          ]

-- | Associates a field name with the arguments it will be passed in the query.
--
-- https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
data FieldCall = FieldCall
  { fcName :: !G.Name,
    fcArguments :: !RemoteArguments
  }
  deriving (Show, Eq, Generic)

instance NFData FieldCall

instance Cacheable FieldCall

instance Hashable FieldCall

-- | Arguments to a remote GraphQL fields, represented as a mapping from name to
-- GraphQL Value. Said values can be variable names, in which case they'll be
-- referring to values we're closed over.
-- TODO: expand on this
newtype RemoteArguments = RemoteArguments
  { getRemoteArguments :: HashMap G.Name (G.Value G.Name)
  }
  deriving (Show, Eq, Generic, Cacheable, NFData)

instance Hashable RemoteArguments

instance FromJSON RemoteArguments where
  parseJSON = prependFailure details . fmap RemoteArguments . withObject "RemoteArguments" parseObjectFieldsToGValue
    where
      details = "Remote arguments are represented by an object that maps each argument name to its value."

      parseObjectFieldsToGValue hashMap =
        HM.fromList <$> for (HM.toList hashMap) \(key, value) -> do
          name <- G.mkName key `onNothing` fail (T.unpack key <> " is an invalid key name")
          parsedValue <- parseValueAsGValue value
          pure (name, parsedValue)

      parseValueAsGValue = \case
        Object obj ->
          G.VObject <$> parseObjectFieldsToGValue obj
        Array array ->
          G.VList . toList <$> traverse parseValueAsGValue array
        String text ->
          case T.uncons text of
            Just ('$', rest)
              | T.null rest -> fail $ "Empty variable name"
              | otherwise -> case G.mkName rest of
                Nothing -> fail $ "Invalid variable name '" <> T.unpack rest <> "'"
                Just name' -> pure $ G.VVariable name'
            _ -> pure (G.VString text)
        Number !scientificNum ->
          pure $ case floatingOrInteger scientificNum of
            -- this number couldn't be interpreted as an integer
            Left (_ :: Float) -> G.VFloat scientificNum
            -- this number was successfully interpreted as an integer
            Right n -> G.VInt n
        Bool !boolean ->
          pure $ G.VBoolean boolean
        Null ->
          pure G.VNull

instance ToJSON RemoteArguments where
  toJSON (RemoteArguments fields) = fieldsToObject fields
    where
      fieldsToObject =
        Object . HM.fromList . map (bimap G.unName gValueToValue) . HM.toList

      gValueToValue =
        \case
          G.VVariable v -> toJSON ("$" <> G.unName v)
          G.VInt i -> toJSON i
          G.VFloat f -> toJSON f
          G.VString s -> toJSON s
          G.VBoolean b -> toJSON b
          G.VNull -> Null
          G.VEnum s -> toJSON s
          G.VList list -> toJSON (map gValueToValue list)
          G.VObject obj -> fieldsToObject obj

--------------------------------------------------------------------------------
-- template haskell generation

$(makeLenses ''ToSchemaRelationshipDef)
$(deriveJSON hasuraJSON ''ToSchemaRelationshipDef)
