{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Metadata representation of a logical model in the metadata,
--   as well as a parser and prettyprinter for the query code.
module Hasura.LogicalModel.Metadata
  ( LogicalModelName (..),
    LogicalModelMetadata (..),
    lmmArguments,
    lmmCode,
    lmmDescription,
    lmmReturns,
    lmmRootFieldName,
    lmmSelectPermissions,
    LogicalModelArgumentName (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    module Hasura.LogicalModel.Types,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Bifunctor (first)
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Data.Text qualified as T
import Hasura.CustomReturnType (CustomReturnType)
import Hasura.LogicalModel.Types
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Permission (SelPermDef, _pdRole)
import Hasura.SQL.Backend
import Hasura.Session (RoleName)

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
  deriving stock (Eq, Ord, Show, Functor, Foldable, Data, Generic, Traversable)

-- | Converting an interpolated query back to text.
--   Should roundtrip with the 'parseInterpolatedQuery'.
ppInterpolatedItem :: InterpolatedItem LogicalModelArgumentName -> Text
ppInterpolatedItem (IIText t) = t
ppInterpolatedItem (IIVariable v) = "{{" <> getLogicalModelArgumentName v <> "}}"

deriving instance (Hashable variable) => Hashable (InterpolatedItem variable)

deriving instance (NFData variable) => NFData (InterpolatedItem variable)

---------------------------------------

-- | A list of logical model components representing a single logical model,
--   separating the variables from the text.
newtype InterpolatedQuery variable = InterpolatedQuery
  { getInterpolatedQuery :: [InterpolatedItem variable]
  }
  deriving newtype (Eq, Ord, Show, Generic)
  deriving stock (Data, Functor, Foldable, Traversable)

deriving newtype instance (Hashable variable) => Hashable (InterpolatedQuery variable)

deriving newtype instance (NFData variable) => NFData (InterpolatedQuery variable)

ppInterpolatedQuery :: InterpolatedQuery LogicalModelArgumentName -> Text
ppInterpolatedQuery (InterpolatedQuery parts) = foldMap ppInterpolatedItem parts

-- | We store the interpolated query as the user text and parse it back
--   when converting back to Haskell code.
instance v ~ LogicalModelArgumentName => HasCodec (InterpolatedQuery v) where
  codec =
    CommentCodec
      ("An interpolated query expressed in native code (SQL)")
      $ bimapCodec
        (first T.unpack . parseInterpolatedQuery)
        ppInterpolatedQuery
        textCodec

deriving via
  (Autodocodec (InterpolatedQuery LogicalModelArgumentName))
  instance
    v ~ LogicalModelArgumentName =>
    ToJSON (InterpolatedQuery v)

---------------------------------------

newtype LogicalModelArgumentName = LogicalModelArgumentName
  { getLogicalModelArgumentName :: Text
  }
  deriving newtype (Eq, Ord, Show, Hashable)
  deriving stock (Generic)

instance HasCodec LogicalModelArgumentName where
  codec = dimapCodec LogicalModelArgumentName getLogicalModelArgumentName codec

deriving newtype instance ToJSON LogicalModelArgumentName

deriving newtype instance FromJSON LogicalModelArgumentName

deriving newtype instance ToJSONKey LogicalModelArgumentName

deriving newtype instance FromJSONKey LogicalModelArgumentName

instance NFData LogicalModelArgumentName

---------------------------------------

-- | The representation of logical models within the metadata structure.
data LogicalModelMetadata (b :: BackendType) = LogicalModelMetadata
  { _lmmRootFieldName :: LogicalModelName,
    _lmmCode :: InterpolatedQuery LogicalModelArgumentName,
    _lmmReturns :: CustomReturnType b,
    _lmmArguments :: HashMap LogicalModelArgumentName (ScalarType b),
    _lmmSelectPermissions :: InsOrdHashMap RoleName (SelPermDef b),
    _lmmDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Eq (LogicalModelMetadata b)

deriving instance Backend b => Show (LogicalModelMetadata b)

instance (Backend b) => HasCodec (LogicalModelMetadata b) where
  codec =
    CommentCodec
      ("A logical model as represented in metadata.")
      $ AC.object (codecNamePrefix @b <> "LogicalModelMetadata")
      $ LogicalModelMetadata
        <$> requiredField "root_field_name" fieldNameDoc
          AC..= _lmmRootFieldName
        <*> requiredField "code" sqlDoc
          AC..= _lmmCode
        <*> requiredField "returns" returnsDoc
          AC..= _lmmReturns
        <*> optionalFieldWithDefault "arguments" mempty argumentDoc
          AC..= _lmmArguments
        <*> optSortedList "select_permissions" _pdRole
          AC..= _lmmSelectPermissions
        <*> optionalField "description" descriptionDoc
          AC..= _lmmDescription
    where
      fieldNameDoc = "Root field name for the logical model"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the logical model which appears in the graphql schema"

      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (LogicalModelMetadata b))
  instance
    (Backend b) => (FromJSON (LogicalModelMetadata b))

deriving via
  (Autodocodec (LogicalModelMetadata b))
  instance
    (Backend b) => (ToJSON (LogicalModelMetadata b))

-- | extract all of the `{{ variable }}` inside our query string
parseInterpolatedQuery ::
  Text ->
  Either Text (InterpolatedQuery LogicalModelArgumentName)
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

    consumeString :: String -> Either Text [InterpolatedItem LogicalModelArgumentName]
    consumeString str =
      let (beforeCurly, fromCurly) = break (== '{') str
       in case fromCurly of
            ('{' : '{' : rest) ->
              (IIText (T.pack beforeCurly) :) <$> consumeVar rest
            ('{' : other) ->
              (IIText (T.pack (beforeCurly <> "{")) :) <$> consumeString other
            _other -> pure [IIText (T.pack beforeCurly)]

    consumeVar :: String -> Either Text [InterpolatedItem LogicalModelArgumentName]
    consumeVar str =
      let (beforeCloseCurly, fromClosedCurly) = break (== '}') str
       in case fromClosedCurly of
            ('}' : '}' : rest) ->
              (IIVariable (LogicalModelArgumentName $ T.pack beforeCloseCurly) :) <$> consumeString rest
            _ -> Left "Found '{{' without a matching closing '}}'"

makeLenses ''LogicalModelMetadata
