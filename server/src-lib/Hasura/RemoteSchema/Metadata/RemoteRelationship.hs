{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.RemoteSchema.Metadata.RemoteRelationship
  ( ToSchemaRelationshipDef (..),
    trrdRemoteField,
    trrdLhsFields,
    trrdRemoteSchema,
    FieldCall (..),
    RemoteArguments (..),
    RemoteFields (..),
    SchemaRemoteRelationships,
    RemoteSchemaTypeRelationships (..),
    rstrsName,
    rstrsRelationships,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH qualified as J
import Data.Aeson.Types (prependFailure)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RemoteSchema.Metadata.Base
import Language.GraphQL.Draft.Syntax qualified as G

-- | Metadata representation of a relationship to a remote schema.
data ToSchemaRelationshipDef = ToSchemaRelationshipDef
  { -- | Identifier for this mapping.
    _trrdRemoteSchema :: RemoteSchemaName,
    -- | The lhs fields that must be forwarded to the remote schema.
    _trrdLhsFields :: HashSet FieldName,
    _trrdRemoteField :: RemoteFields
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

instance J.FromJSON RemoteFields where
  parseJSON = prependFailure details . fmap RemoteFields . parseRemoteFields
    where
      details = "Remote fields are represented by an object that maps each field name to its arguments."
      parseRemoteFields = J.withObject "RemoteFields" \hashmap -> case KM.toList hashmap of
        [(fieldNameKey, callValue)] -> do
          fieldName <- J.parseJSON $ J.String $ K.toText fieldNameKey
          callObject <- J.parseJSON callValue
          arguments <- callObject J..: "arguments"
          maybeSubField <- callObject J..:? "field"
          subFields <-
            fromMaybe [] <$> for maybeSubField \fieldValue -> do
              remoteFields <- parseRemoteFields fieldValue
              pure (toList remoteFields)
          pure $ FieldCall {fcName = fieldName, fcArguments = arguments} :| subFields
        [] -> fail "Expecting one single mapping, received none."
        _ -> fail "Expecting one single mapping, received too many."

instance J.ToJSON RemoteFields where
  toJSON (RemoteFields fields) = remoteFieldsJson fields
    where
      remoteFieldsJson (field :| subfields) =
        J.object
          [ K.fromText (G.unName (fcName field))
              J..= J.object
                ( catMaybes
                    [ Just $ "arguments" J..= fcArguments field,
                      nonEmpty subfields <&> \sf -> "field" J..= remoteFieldsJson sf
                    ]
                )
          ]

-- | Associates a field name with the arguments it will be passed in the query.
--
-- https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
data FieldCall = FieldCall
  { fcName :: G.Name,
    fcArguments :: RemoteArguments
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

instance J.FromJSON RemoteArguments where
  parseJSON = prependFailure details . fmap RemoteArguments . J.withObject "RemoteArguments" parseObjectFieldsToGValue
    where
      details = "Remote arguments are represented by an object that maps each argument name to its value."

      parseObjectFieldsToGValue keyMap =
        HM.fromList <$> for (KM.toList keyMap) \(K.toText -> key, value) -> do
          name <- G.mkName key `onNothing` fail (T.unpack key <> " is an invalid key name")
          parsedValue <- parseValueAsGValue value
          pure (name, parsedValue)

      parseValueAsGValue = \case
        J.Object obj ->
          G.VObject <$> parseObjectFieldsToGValue obj
        J.Array array ->
          G.VList . toList <$> traverse parseValueAsGValue array
        J.String text ->
          case T.uncons text of
            Just ('$', rest)
              | T.null rest -> fail $ "Empty variable name"
              | otherwise -> case G.mkName rest of
                Nothing -> fail $ "Invalid variable name '" <> T.unpack rest <> "'"
                Just name' -> pure $ G.VVariable name'
            _ -> pure (G.VString text)
        J.Number !scientificNum ->
          pure $ case floatingOrInteger scientificNum of
            -- this number couldn't be interpreted as an integer
            Left (_ :: Float) -> G.VFloat scientificNum
            -- this number was successfully interpreted as an integer
            Right n -> G.VInt n
        J.Bool !boolean ->
          pure $ G.VBoolean boolean
        J.Null ->
          pure G.VNull

instance J.ToJSON RemoteArguments where
  toJSON (RemoteArguments fields) = fieldsToObject fields
    where
      fieldsToObject =
        J.Object . KM.fromList . map (bimap (K.fromText . G.unName) gValueToValue) . HM.toList

      gValueToValue =
        \case
          G.VVariable v -> J.toJSON ("$" <> G.unName v)
          G.VInt i -> J.toJSON i
          G.VFloat f -> J.toJSON f
          G.VString s -> J.toJSON s
          G.VBoolean b -> J.toJSON b
          G.VNull -> J.Null
          G.VEnum s -> J.toJSON s
          G.VList list -> J.toJSON (map gValueToValue list)
          G.VObject obj -> fieldsToObject obj

type RemoteRelationships r = InsOrdHashMap RelName (RemoteRelationshipG r)

data RemoteSchemaTypeRelationships r = RemoteSchemaTypeRelationships
  { _rstrsName :: G.Name,
    _rstrsRelationships :: RemoteRelationships r
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON (RemoteRelationshipG r) => J.FromJSON (RemoteSchemaTypeRelationships r) where
  parseJSON = J.withObject "RemoteSchemaMetadata" \obj ->
    RemoteSchemaTypeRelationships
      <$> obj J..: "type_name"
      <*> (oMapFromL _rrName <$> obj J..:? "relationships" J..!= [])

instance J.ToJSON (RemoteRelationshipG r) => J.ToJSON (RemoteSchemaTypeRelationships r) where
  toJSON RemoteSchemaTypeRelationships {..} =
    J.object
      [ "type_name" J..= _rstrsName,
        "relationships" J..= OM.elems _rstrsRelationships
      ]

instance Cacheable r => Cacheable (RemoteSchemaTypeRelationships r)

type SchemaRemoteRelationships r = InsOrdHashMap G.Name (RemoteSchemaTypeRelationships r)

$(J.deriveJSON hasuraJSON ''ToSchemaRelationshipDef)
$(makeLenses ''RemoteSchemaTypeRelationships)
$(makeLenses ''ToSchemaRelationshipDef)
