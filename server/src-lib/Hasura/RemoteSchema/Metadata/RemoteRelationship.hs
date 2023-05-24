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

import Autodocodec
import Autodocodec qualified as AC
import Autodocodec.Extended (graphQLFieldNameCodec, graphQLValueCodec, hashSetCodec, typeableName)
import Control.Exception.Safe (Typeable)
import Control.Lens (makeLenses)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH qualified as J
import Data.Aeson.Types (prependFailure)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd.Autodocodec (insertionOrderedElemsCodec)
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
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

instance HasCodec ToSchemaRelationshipDef where
  codec =
    object "ToSchemaRelationshipDef"
      $ ToSchemaRelationshipDef
      <$> requiredField' "remote_schema"
      .= _trrdRemoteSchema
        <*> requiredFieldWith' "lhs_fields" hashSetCodec
      .= _trrdLhsFields
        <*> requiredField' "remote_field"
      .= _trrdRemoteField

-- | Targeted field in a remote schema relationship.
-- TODO: explain about subfields and why this is a container
newtype RemoteFields = RemoteFields {unRemoteFields :: NonEmpty FieldCall}
  deriving (Show, Eq, Generic)

instance NFData RemoteFields

instance HasCodec RemoteFields where
  codec =
    named "RemoteFields"
      $ bimapCodec dec enc
      $ hashMapCodec argumentsCodec
      <?> "Remote fields are represented by an object that maps each field name to its arguments."
    where
      argumentsCodec :: JSONCodec (RemoteArguments, Maybe RemoteFields)
      argumentsCodec =
        object "FieldCall"
          $ (,)
          <$> requiredField' "arguments"
          .= fst
            <*> optionalField' "field"
          .= snd

      dec :: HashMap G.Name (RemoteArguments, Maybe RemoteFields) -> Either String RemoteFields
      dec hashmap = case HashMap.toList hashmap of
        [(fieldName, (arguments, maybeSubField))] ->
          let subfields = maybe [] (toList . unRemoteFields) maybeSubField
           in Right
                $ RemoteFields
                $ FieldCall {fcName = fieldName, fcArguments = arguments}
                :| subfields
        [] -> Left "Expecting one single mapping, received none."
        _ -> Left "Expecting one single mapping, received too many."

      enc :: RemoteFields -> HashMap G.Name (RemoteArguments, Maybe RemoteFields)
      enc (RemoteFields (field :| subfields)) =
        HashMap.singleton (fcName field) (fcArguments field, RemoteFields <$> nonEmpty subfields)

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

instance Hashable FieldCall

-- | Arguments to a remote GraphQL fields, represented as a mapping from name to
-- GraphQL Value. Said values can be variable names, in which case they'll be
-- referring to values we're closed over.
-- TODO: expand on this
newtype RemoteArguments = RemoteArguments
  { getRemoteArguments :: HashMap G.Name (G.Value G.Name)
  }
  deriving (Show, Eq, Generic, NFData)

instance Hashable RemoteArguments

instance HasCodec RemoteArguments where
  codec =
    named "RemoteArguments"
      $ CommentCodec "Remote arguments are represented by an object that maps each argument name to its value."
      $ dimapCodec RemoteArguments getRemoteArguments
      $ hashMapCodec (graphQLValueCodec varCodec)
    where
      varCodec = bimapCodec decodeVariable encodeVariable textCodec

      decodeVariable text = case T.uncons text of
        Just ('$', rest)
          | T.null rest -> Left $ "Empty variable name"
          | otherwise ->
              onNothing
                (G.mkName rest)
                (Left $ "Invalid variable name '" <> T.unpack rest <> "'")
        _ -> Left $ "Variable name must start with $"

      encodeVariable name = "$" <> G.unName name

instance J.FromJSON RemoteArguments where
  parseJSON = prependFailure details . fmap RemoteArguments . J.withObject "RemoteArguments" parseObjectFieldsToGValue
    where
      details = "Remote arguments are represented by an object that maps each argument name to its value."

      parseObjectFieldsToGValue keyMap =
        HashMap.fromList <$> for (KM.toList keyMap) \(K.toText -> key, value) -> do
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
        J.Object . KM.fromList . map (bimap (K.fromText . G.unName) gValueToValue) . HashMap.toList

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

instance (HasCodec (RemoteRelationshipG r), Typeable r) => HasCodec (RemoteSchemaTypeRelationships r) where
  codec =
    AC.object ("RemoteSchemaMetadata_" <> typeableName @r)
      $ RemoteSchemaTypeRelationships
      <$> requiredFieldWith' "type_name" graphQLFieldNameCodec
      AC..= _rstrsName
        <*> optionalFieldWithDefaultWith'
          "relationships"
          (insertionOrderedElemsCodec _rrName)
          mempty
      AC..= _rstrsRelationships

instance (J.FromJSON (RemoteRelationshipG r)) => J.FromJSON (RemoteSchemaTypeRelationships r) where
  parseJSON = J.withObject "RemoteSchemaMetadata" \obj ->
    RemoteSchemaTypeRelationships
      <$> obj
      J..: "type_name"
      <*> (oMapFromL _rrName <$> obj J..:? "relationships" J..!= [])

instance (J.ToJSON (RemoteRelationshipG r)) => J.ToJSON (RemoteSchemaTypeRelationships r) where
  toJSON RemoteSchemaTypeRelationships {..} =
    J.object
      [ "type_name" J..= _rstrsName,
        "relationships" J..= InsOrdHashMap.elems _rstrsRelationships
      ]

type SchemaRemoteRelationships r = InsOrdHashMap G.Name (RemoteSchemaTypeRelationships r)

$(J.deriveJSON hasuraJSON ''ToSchemaRelationshipDef)
$(makeLenses ''RemoteSchemaTypeRelationships)
$(makeLenses ''ToSchemaRelationshipDef)
