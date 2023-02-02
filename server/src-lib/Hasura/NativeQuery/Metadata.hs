{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module houses the types and functions associated with the default
-- implementation of the metadata of native queries.
module Hasura.NativeQuery.Metadata
  ( NativeQueryArgumentName (..),
    NativeQueryNameImpl (..),
    NativeQueryInfoImpl (..),
    TrackNativeQueryImpl (..),
    RawQuery (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    ppInterpolatedQuery,
    showInterpolatedQuery,
    defaultNativeQueryTrackToInfo,
    module Hasura.NativeQuery.Types,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Data.Aeson
import Data.Bifunctor (first)
import Data.Text qualified as T
import Data.Text.Extended (ToTxt)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.NativeQuery.Types
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend

-- The name of a native query. This appears as a root field name in the graphql schema.
newtype NativeQueryNameImpl = NativeQueryNameImpl {getNativeQueryNameImpl :: Text}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Generic)

instance HasCodec NativeQueryNameImpl where
  codec = dimapCodec NativeQueryNameImpl getNativeQueryNameImpl codec

instance FromJSONKey NativeQueryNameImpl

instance ToJSONKey NativeQueryNameImpl

---------------------------------------

newtype RawQuery = RawQuery {getRawQuery :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

instance HasCodec RawQuery where
  codec = AC.dimapCodec RawQuery getRawQuery codec

---------------------------------------

-- | A component of an interpolated query
data InterpolatedItem variable
  = -- | normal text
    IIText Text
  | -- | a captured variable
    IIVariable variable
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Traversable)

-- | Converting an interpolated query back to text.
--   Should roundtrip with the 'parseInterpolatedQuery'.
ppInterpolatedItem :: InterpolatedItem NativeQueryArgumentName -> Text
ppInterpolatedItem (IIText t) = t
ppInterpolatedItem (IIVariable v) = "{{" <> getNativeQueryArgumentName v <> "}}"

-- | Converting an interpolated query back to text.
--   Uses @Show@ to print the variables.
showInterpolatedItem :: Show variable => InterpolatedItem variable -> Text
showInterpolatedItem (IIText t) = t
showInterpolatedItem (IIVariable v) = "{{" <> tshow v <> "}}"

deriving instance (Hashable variable) => Hashable (InterpolatedItem variable)

deriving instance (NFData variable) => NFData (InterpolatedItem variable)

---------------------------------------

-- | A list of native query components representing a single native query,
--   separating the variables from the text.
newtype InterpolatedQuery variable = InterpolatedQuery
  { getInterpolatedQuery :: [InterpolatedItem variable]
  }
  deriving newtype (Eq, Ord, Show, Generic)
  deriving stock (Functor, Foldable, Traversable)

deriving newtype instance (Hashable variable) => Hashable (InterpolatedQuery variable)

deriving newtype instance (NFData variable) => NFData (InterpolatedQuery variable)

ppInterpolatedQuery :: InterpolatedQuery NativeQueryArgumentName -> Text
ppInterpolatedQuery (InterpolatedQuery parts) = foldMap ppInterpolatedItem parts

showInterpolatedQuery :: Show variable => InterpolatedQuery variable -> Text
showInterpolatedQuery (InterpolatedQuery parts) = foldMap showInterpolatedItem parts

-- | We store the interpolated query as the user text and parse it back
--   when converting back to Haskell code.
instance HasCodec (InterpolatedQuery NativeQueryArgumentName) where
  codec =
    CommentCodec
      ("An interpolated query expressed in native code (SQL)")
      $ bimapCodec
        (first T.unpack . parseInterpolatedQuery)
        ppInterpolatedQuery
        textCodec

---------------------------------------

-- | Default implementation of the Native Query metadata info object.
data NativeQueryInfoImpl (b :: BackendType) = NativeQueryInfoImpl
  { nqiiRootFieldName :: NativeQueryNameImpl,
    nqiiCode :: InterpolatedQuery NativeQueryArgumentName,
    nqiiReturns :: TableName b,
    nqiiArguments :: HashMap NativeQueryArgumentName (ScalarType b),
    nqiiDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Eq (NativeQueryInfoImpl b)

deriving instance Backend b => Show (NativeQueryInfoImpl b)

instance Backend b => Hashable (NativeQueryInfoImpl b)

instance Backend b => NFData (NativeQueryInfoImpl b)

instance (Backend b, HasCodec (ScalarType b)) => HasCodec (NativeQueryInfoImpl b) where
  codec =
    CommentCodec
      ("A query in expressed in native code (SQL) to add to the GraphQL schema with configuration.")
      $ AC.object (codecNamePrefix @b <> "NativeQueryInfo")
      $ NativeQueryInfoImpl
        <$> requiredField "root_field_name" fieldNameDoc
          AC..= nqiiRootFieldName
        <*> requiredField "code" sqlDoc
          AC..= nqiiCode
        <*> requiredField "returns" returnsDoc
          AC..= nqiiReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
          AC..= nqiiArguments
        <*> optionalField "description" descriptionDoc
          AC..= nqiiDescription
    where
      fieldNameDoc = "Root field name for the native query"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (Autodocodec (NativeQueryInfoImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => (FromJSON (NativeQueryInfoImpl b))

deriving via
  (Autodocodec (NativeQueryInfoImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => (ToJSON (NativeQueryInfoImpl b))

newtype NativeQueryArgumentName = NativeQueryArgumentName {getNativeQueryArgumentName :: Text}
  deriving newtype (Eq, Ord, Show, Hashable)
  deriving stock (Generic)

deriving newtype instance ToJSON NativeQueryArgumentName

deriving newtype instance FromJSON NativeQueryArgumentName

deriving newtype instance ToJSONKey NativeQueryArgumentName

deriving newtype instance FromJSONKey NativeQueryArgumentName

instance NFData (NativeQueryArgumentName)

-- | Default implementation of the 'track_native_query' request payload.
data TrackNativeQueryImpl (b :: BackendType) = TrackNativeQueryImpl
  { tnqSource :: SourceName,
    tnqRootFieldName :: NativeQueryNameImpl,
    tnqCode :: Text,
    tnqArguments :: HashMap NativeQueryArgumentName (ScalarType b),
    tnqDescription :: Maybe Text,
    tnqReturns :: TableName b
  }

-- | Default implementation of the method 'nativeQueryTrackToInfo'.
defaultNativeQueryTrackToInfo :: TrackNativeQueryImpl b -> Either NativeQueryParseError (NativeQueryInfoImpl b)
defaultNativeQueryTrackToInfo TrackNativeQueryImpl {..} = do
  nqiiCode <- mapLeft NativeQueryParseError (parseInterpolatedQuery tnqCode)
  pure $ NativeQueryInfoImpl {..}
  where
    nqiiRootFieldName = tnqRootFieldName
    nqiiReturns = tnqReturns
    nqiiArguments = tnqArguments
    nqiiDescription = tnqDescription

instance (Backend b, HasCodec (ScalarType b)) => HasCodec (TrackNativeQueryImpl b) where
  codec =
    CommentCodec
      ("A request to track a native query")
      $ AC.object (codecNamePrefix @b <> "TrackNativeQuery")
      $ TrackNativeQueryImpl
        <$> requiredField "source" sourceDoc
          AC..= tnqSource
        <*> requiredField "root_field_name" rootFieldDoc
          AC..= tnqRootFieldName
        <*> requiredField "code" codeDoc
          AC..= tnqCode
        <*> optionalFieldWithDefault "arguments" mempty argumentsDoc
          AC..= tnqArguments
        <*> optionalField "description" descriptionDoc
          AC..= tnqDescription
        <*> requiredField "returns" returnsDoc
          AC..= tnqReturns
    where
      sourceDoc = "The source in whic this native query should be tracked"
      rootFieldDoc = "Root field name for the native query"
      codeDoc = "Native code expression (SQL) to run"
      argumentsDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (Autodocodec (TrackNativeQueryImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => FromJSON (TrackNativeQueryImpl b)

deriving via
  (Autodocodec (TrackNativeQueryImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => ToJSON (TrackNativeQueryImpl b)

-- | extract all of the `{{ variable }}` inside our query string
parseInterpolatedQuery ::
  Text ->
  Either Text (InterpolatedQuery NativeQueryArgumentName)
parseInterpolatedQuery =
  fmap
    ( InterpolatedQuery
        . mergeAdjacent
        . trashEmpties
    )
    . consumeString
    . T.unpack
  where
    trashEmpties = filter (/= IIText "")

    mergeAdjacent = \case
      (IIText a : IIText b : rest) ->
        mergeAdjacent (IIText (a <> b) : rest)
      (a : rest) -> a : mergeAdjacent rest
      [] -> []

    consumeString :: String -> Either Text [InterpolatedItem NativeQueryArgumentName]
    consumeString str =
      let (beforeCurly, fromCurly) = break (== '{') str
       in case fromCurly of
            ('{' : '{' : rest) ->
              (IIText (T.pack beforeCurly) :) <$> consumeVar rest
            ('{' : other) ->
              (IIText (T.pack (beforeCurly <> "{")) :) <$> consumeString other
            _other -> pure [IIText (T.pack beforeCurly)]

    consumeVar :: String -> Either Text [InterpolatedItem NativeQueryArgumentName]
    consumeVar str =
      let (beforeCloseCurly, fromClosedCurly) = break (== '}') str
       in case fromClosedCurly of
            ('}' : '}' : rest) ->
              (IIVariable (NativeQueryArgumentName $ T.pack beforeCloseCurly) :) <$> consumeString rest
            _ -> Left "Found '{{' without a matching closing '}}'"
