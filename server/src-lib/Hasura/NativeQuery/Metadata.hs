{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Metadata representation of a native query in the metadata,
--   as well as a parser and prettyprinter for the query code.
module Hasura.NativeQuery.Metadata
  ( NativeQueryName (..),
    NativeQueryMetadata (..),
    nqmArguments,
    nqmCode,
    nqmDescription,
    nqmReturns,
    nqmArrayRelationships,
    nqmRootFieldName,
    NativeQueryArgumentName (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    module Hasura.NativeQuery.Types,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifunctor (first)
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.LogicalModel.Types
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.NativeQuery.Types (NativeQueryName (..), NullableScalarType (..), nullableScalarTypeMapCodec)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelDef (..), RelManualConfig (..))
import Hasura.SQL.Backend
import Language.Haskell.TH.Syntax (Lift)

-- | copy pasta'd from Hasura.RQL.Types.Metadata.Common, forgive me Padre i did
-- not have the heart for the Real Fix.
type Relationships = InsOrdHashMap RelName

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
  deriving stock (Eq, Ord, Show, Functor, Foldable, Data, Generic, Lift, Traversable)

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
  deriving stock (Data, Functor, Foldable, Lift, Traversable)

deriving newtype instance (Hashable variable) => Hashable (InterpolatedQuery variable)

deriving newtype instance (NFData variable) => NFData (InterpolatedQuery variable)

ppInterpolatedQuery :: InterpolatedQuery NativeQueryArgumentName -> Text
ppInterpolatedQuery (InterpolatedQuery parts) = foldMap ppInterpolatedItem parts

-- | We store the interpolated query as the user text and parse it back
--   when converting back to Haskell code.
instance v ~ NativeQueryArgumentName => HasCodec (InterpolatedQuery v) where
  codec =
    CommentCodec
      ("An interpolated query expressed in native code (SQL)")
      $ bimapCodec
        (first T.unpack . parseInterpolatedQuery)
        ppInterpolatedQuery
        textCodec

deriving via
  (Autodocodec (InterpolatedQuery NativeQueryArgumentName))
  instance
    v ~ NativeQueryArgumentName =>
    ToJSON (InterpolatedQuery v)

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

instance NFData NativeQueryArgumentName

---------------------------------------

-- | The representation of native queries within the metadata structure.
data NativeQueryMetadata (b :: BackendType) = NativeQueryMetadata
  { _nqmRootFieldName :: NativeQueryName,
    _nqmCode :: InterpolatedQuery NativeQueryArgumentName,
    _nqmReturns :: LogicalModelName,
    _nqmArguments :: HashMap NativeQueryArgumentName (NullableScalarType b),
    _nqmArrayRelationships :: Relationships (RelDef (RelManualConfig b)),
    _nqmDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Eq (NativeQueryMetadata b)

deriving instance Backend b => Show (NativeQueryMetadata b)

instance (Backend b) => HasCodec (NativeQueryMetadata b) where
  codec =
    CommentCodec
      ("A native query as represented in metadata.")
      $ AC.object (codecNamePrefix @b <> "NativeQueryMetadata")
      $ NativeQueryMetadata
        <$> requiredField "root_field_name" fieldNameDoc
          AC..= _nqmRootFieldName
        <*> requiredField "code" sqlDoc
          AC..= _nqmCode
        <*> requiredField "returns" returnsDoc
          AC..= _nqmReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
          AC..= _nqmArguments
        <*> optSortedList "array_relationships" _rdName
          AC..= _nqmArrayRelationships
        <*> optionalField "description" descriptionDoc
          AC..= _nqmDescription
    where
      fieldNameDoc = "Root field name for the native query"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the native query which appears in the graphql schema"

      optSortedList ::
        (HasCodec a, Eq a, Hashable k, Ord k, T.ToTxt k) =>
        Text ->
        (a -> k) ->
        ObjectCodec (InsOrdHashMap k a) (InsOrdHashMap k a)
      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (NativeQueryMetadata b))
  instance
    (Backend b) => (FromJSON (NativeQueryMetadata b))

deriving via
  (Autodocodec (NativeQueryMetadata b))
  instance
    (Backend b) => (ToJSON (NativeQueryMetadata b))

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

makeLenses ''NativeQueryMetadata
