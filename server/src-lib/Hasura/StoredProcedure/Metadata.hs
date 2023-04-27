{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Metadata representation of a stored procedure in the metadata,
--   as well as a parser and prettyprinter for the query code.
module Hasura.StoredProcedure.Metadata
  ( StoredProcedureName (..),
    StoredProcedureMetadata (..),
    spmArguments,
    spmCode,
    spmDescription,
    spmReturns,
    spmArrayRelationships,
    spmRootFieldName,
    StoredProcedureArgumentName (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    module Hasura.StoredProcedure.Types,
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
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelDef (..), RelManualConfig (..))
import Hasura.StoredProcedure.Types (NullableScalarType (..), StoredProcedureName (..), nullableScalarTypeMapCodec)
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
ppInterpolatedItem :: InterpolatedItem StoredProcedureArgumentName -> Text
ppInterpolatedItem (IIText t) = t
ppInterpolatedItem (IIVariable v) = "{{" <> getStoredProcedureArgumentName v <> "}}"

deriving instance (Hashable variable) => Hashable (InterpolatedItem variable)

deriving instance (NFData variable) => NFData (InterpolatedItem variable)

---------------------------------------

-- | A list of stored procedure components representing a single stored procedure,
--   separating the variables from the text.
newtype InterpolatedQuery variable = InterpolatedQuery
  { getInterpolatedQuery :: [InterpolatedItem variable]
  }
  deriving newtype (Eq, Ord, Show, Generic)
  deriving stock (Data, Functor, Foldable, Lift, Traversable)

deriving newtype instance (Hashable variable) => Hashable (InterpolatedQuery variable)

deriving newtype instance (NFData variable) => NFData (InterpolatedQuery variable)

ppInterpolatedQuery :: InterpolatedQuery StoredProcedureArgumentName -> Text
ppInterpolatedQuery (InterpolatedQuery parts) = foldMap ppInterpolatedItem parts

-- | We store the interpolated query as the user text and parse it back
--   when converting back to Haskell code.
instance v ~ StoredProcedureArgumentName => HasCodec (InterpolatedQuery v) where
  codec =
    CommentCodec
      ("An interpolated query expressed in native code (SQL)")
      $ bimapCodec
        (first T.unpack . parseInterpolatedQuery)
        ppInterpolatedQuery
        textCodec

deriving via
  (Autodocodec (InterpolatedQuery StoredProcedureArgumentName))
  instance
    v ~ StoredProcedureArgumentName =>
    ToJSON (InterpolatedQuery v)

---------------------------------------

newtype StoredProcedureArgumentName = StoredProcedureArgumentName
  { getStoredProcedureArgumentName :: Text
  }
  deriving newtype (Eq, Ord, Show, Hashable)
  deriving stock (Generic)

instance HasCodec StoredProcedureArgumentName where
  codec = dimapCodec StoredProcedureArgumentName getStoredProcedureArgumentName codec

deriving newtype instance ToJSON StoredProcedureArgumentName

deriving newtype instance FromJSON StoredProcedureArgumentName

deriving newtype instance ToJSONKey StoredProcedureArgumentName

deriving newtype instance FromJSONKey StoredProcedureArgumentName

instance NFData StoredProcedureArgumentName

---------------------------------------

-- | The representation of native queries within the metadata structure.
data StoredProcedureMetadata (b :: BackendType) = StoredProcedureMetadata
  { _spmRootFieldName :: StoredProcedureName,
    _spmCode :: InterpolatedQuery StoredProcedureArgumentName,
    _spmReturns :: LogicalModelName,
    _spmArguments :: HashMap StoredProcedureArgumentName (NullableScalarType b),
    _spmArrayRelationships :: Relationships (RelDef (RelManualConfig b)),
    _spmDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Eq (StoredProcedureMetadata b)

deriving instance Backend b => Show (StoredProcedureMetadata b)

instance (Backend b) => HasCodec (StoredProcedureMetadata b) where
  codec =
    CommentCodec
      ("A stored procedure as represented in metadata.")
      $ AC.object (backendPrefix @b <> "StoredProcedureMetadata")
      $ StoredProcedureMetadata
        <$> requiredField "root_field_name" fieldNameDoc
          AC..= _spmRootFieldName
        <*> requiredField "code" sqlDoc
          AC..= _spmCode
        <*> requiredField "returns" returnsDoc
          AC..= _spmReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
          AC..= _spmArguments
        <*> optSortedList "array_relationships" _rdName
          AC..= _spmArrayRelationships
        <*> optionalField "description" descriptionDoc
          AC..= _spmDescription
    where
      fieldNameDoc = "Root field name for the stored procedure"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the stored procedure which appears in the graphql schema"

      optSortedList ::
        (HasCodec a, Eq a, Hashable k, Ord k, T.ToTxt k) =>
        Text ->
        (a -> k) ->
        ObjectCodec (InsOrdHashMap k a) (InsOrdHashMap k a)
      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (StoredProcedureMetadata b))
  instance
    (Backend b) => (FromJSON (StoredProcedureMetadata b))

deriving via
  (Autodocodec (StoredProcedureMetadata b))
  instance
    (Backend b) => (ToJSON (StoredProcedureMetadata b))

-- | extract all of the `{{ variable }}` inside our query string
parseInterpolatedQuery ::
  Text ->
  Either Text (InterpolatedQuery StoredProcedureArgumentName)
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

    consumeString :: String -> Either Text [InterpolatedItem StoredProcedureArgumentName]
    consumeString str =
      let (beforeCurly, fromCurly) = break (== '{') str
       in case fromCurly of
            ('{' : '{' : rest) ->
              (IIText (T.pack beforeCurly) :) <$> consumeVar rest
            ('{' : other) ->
              (IIText (T.pack (beforeCurly <> "{")) :) <$> consumeString other
            _other -> pure [IIText (T.pack beforeCurly)]

    consumeVar :: String -> Either Text [InterpolatedItem StoredProcedureArgumentName]
    consumeVar str =
      let (beforeCloseCurly, fromClosedCurly) = break (== '}') str
       in case fromClosedCurly of
            ('}' : '}' : rest) ->
              (IIVariable (StoredProcedureArgumentName $ T.pack beforeCloseCurly) :) <$> consumeString rest
            _ -> Left "Found '{{' without a matching closing '}}'"

makeLenses ''StoredProcedureMetadata
