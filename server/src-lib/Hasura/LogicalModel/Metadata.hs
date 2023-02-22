{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module houses the types and functions associated with the default
-- implementation of the metadata of native queries.
module Hasura.LogicalModel.Metadata
  ( LogicalModelName (..),
    NativeQueryInfo (..),
    NativeQueryArgumentName (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    module Hasura.LogicalModel.Types,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Data.Aeson
import Data.Bifunctor (first)
import Data.Text qualified as T
import Hasura.CustomReturnType (CustomReturnType)
import Hasura.LogicalModel.Types
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend

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

newtype NativeQueryArgumentName = NativeQueryArgumentName
  { getNativeQueryArgumentName :: Text
  }
  deriving newtype (Eq, Ord, Show, Hashable)
  deriving stock (Generic)

instance HasCodec NativeQueryArgumentName where
  codec = dimapCodec NativeQueryArgumentName getNativeQueryArgumentName codec

deriving newtype instance ToJSON NativeQueryArgumentName

deriving newtype instance FromJSON NativeQueryArgumentName

deriving newtype instance ToJSONKey NativeQueryArgumentName

deriving newtype instance FromJSONKey NativeQueryArgumentName

instance NFData (NativeQueryArgumentName)

---------------------------------------

-- | Default implementation of the Native Query metadata info object.
data NativeQueryInfo (b :: BackendType) = NativeQueryInfo
  { nqiRootFieldName :: LogicalModelName,
    nqiCode :: InterpolatedQuery NativeQueryArgumentName,
    nqiReturns :: CustomReturnType b,
    nqiArguments :: HashMap NativeQueryArgumentName (ScalarType b),
    nqiDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Eq (NativeQueryInfo b)

deriving instance Backend b => Show (NativeQueryInfo b)

instance Backend b => Hashable (NativeQueryInfo b)

instance Backend b => NFData (NativeQueryInfo b)

instance (Backend b) => HasCodec (NativeQueryInfo b) where
  codec =
    CommentCodec
      ("A query in expressed in native code (SQL) to add to the GraphQL schema with configuration.")
      $ AC.object (codecNamePrefix @b <> "NativeQueryInfo")
      $ NativeQueryInfo
        <$> requiredField "root_field_name" fieldNameDoc
          AC..= nqiRootFieldName
        <*> requiredField "code" sqlDoc
          AC..= nqiCode
        <*> requiredField "returns" returnsDoc
          AC..= nqiReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
          AC..= nqiArguments
        <*> optionalField "description" descriptionDoc
          AC..= nqiDescription
    where
      fieldNameDoc = "Root field name for the native query"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (Autodocodec (NativeQueryInfo b))
  instance
    (Backend b) => (FromJSON (NativeQueryInfo b))

deriving via
  (Autodocodec (NativeQueryInfo b))
  instance
    (Backend b) => (ToJSON (NativeQueryInfo b))

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
