{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

-- | types and helpers for user-defined-functions after they have been resolved
-- in the schema cache
module Hasura.Function.Cache
  ( DBFunctionsMetadata,
    FunctionOverloads (..),
    FunctionArgName (..),
    FunctionCache,
    FunctionConfig (..),
    FunctionCustomRootFields (..),
    FunctionExposedAs (..),
    FunctionInfo (..),
    FunctionInputArgument,
    FunctionPermissionInfo (..),
    FunctionPermissionsMap,
    FunctionVolatility (..),
    InputArgument (..),
    FunctionArgsExpG (..),
    FunctionArgsExp,
    TrackableFunctionInfo (..),
    TrackableTableInfo (..),
    TrackableInfo (..),
    emptyFunctionConfig,
    emptyFunctionCustomRootFields,
    funcTypToTxt,
    emptyFunctionArgsExp,
  )
where

import Autodocodec (HasCodec (codec))
import Autodocodec qualified as AC
import Autodocodec.Extended (graphQLFieldNameCodec)
import Control.Lens
import Data.Aeson
import Data.Aeson.Casing
import Data.Char (toLower)
import Data.HashMap.Strict qualified as HashMap
import Data.List.Extended as LE
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Roles (RoleName)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax

-- | https://www.postgresql.org/docs/current/xfunc-volatility.html
data FunctionVolatility
  = FTVOLATILE
  | FTIMMUTABLE
  | FTSTABLE
  deriving (Eq, Generic)

instance NFData FunctionVolatility

instance FromJSON FunctionVolatility where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = drop 2}

instance ToJSON FunctionVolatility where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = drop 2}
  toEncoding = genericToEncoding defaultOptions {constructorTagModifier = drop 2}

funcTypToTxt :: FunctionVolatility -> Text
funcTypToTxt FTVOLATILE = "VOLATILE"
funcTypToTxt FTIMMUTABLE = "IMMUTABLE"
funcTypToTxt FTSTABLE = "STABLE"

instance Show FunctionVolatility where
  show = T.unpack . funcTypToTxt

newtype FunctionArgName = FunctionArgName {getFuncArgNameTxt :: Text}
  deriving (Show, Eq, Ord, NFData, ToJSON, ToJSONKey, FromJSON, FromJSONKey, ToTxt, IsString, Generic, Hashable, Lift, Data)

instance AC.HasCodec FunctionArgName where
  codec = AC.dimapCodec FunctionArgName getFuncArgNameTxt codec

data InputArgument a
  = IAUserProvided a
  | IASessionVariables FunctionArgName
  deriving (Show, Eq, Functor, Generic)

instance (ToJSON a) => ToJSON (InputArgument a) where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = snakeCase . drop 2, sumEncoding = TaggedObject "type" "argument"}
  toEncoding = genericToEncoding defaultOptions {constructorTagModifier = snakeCase . drop 2, sumEncoding = TaggedObject "type" "argument"}

type FunctionInputArgument b = InputArgument (FunctionArgument b)

-- | Indicates whether the user requested the corresponding function to be
-- tracked as a mutation or a query/subscription, in @track_function@.
data FunctionExposedAs = FEAQuery | FEAMutation
  deriving (Show, Eq, Generic)

instance NFData FunctionExposedAs

instance HasCodec FunctionExposedAs where
  codec = AC.stringConstCodec [(FEAQuery, "query"), (FEAMutation, "mutation")]

instance FromJSON FunctionExposedAs where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 3}

instance ToJSON FunctionExposedAs where
  toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 3}
  toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 3}

newtype FunctionPermissionInfo = FunctionPermissionInfo
  { _fpmRole :: RoleName
  }
  deriving (Show, Eq, Generic)

instance HasCodec FunctionPermissionInfo where
  codec =
    AC.object "FunctionPermissionInfo"
      $ FunctionPermissionInfo
      <$> AC.requiredField' "role"
      AC..= _fpmRole

instance FromJSON FunctionPermissionInfo where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON FunctionPermissionInfo where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

type FunctionPermissionsMap = HashMap RoleName FunctionPermissionInfo

-- | Custom root fields for functions. When set, will be the names exposed
--   to the user in the schema.
--
--   See rfcs/function-root-field-customisation.md for more information.
data FunctionCustomRootFields = FunctionCustomRootFields
  { _fcrfFunction :: Maybe G.Name,
    _fcrfFunctionAggregate :: Maybe G.Name
  }
  deriving (Show, Eq, Generic)

instance NFData FunctionCustomRootFields

instance HasCodec FunctionCustomRootFields where
  codec =
    AC.bimapCodec checkForDup id
      $ AC.object "FunctionCustomRootFields"
      $ FunctionCustomRootFields
      <$> AC.optionalFieldWith' "function" graphQLFieldNameCodec
      AC..= _fcrfFunction
        <*> AC.optionalFieldWith' "function_aggregate" graphQLFieldNameCodec
      AC..= _fcrfFunctionAggregate
    where
      checkForDup (FunctionCustomRootFields (Just f) (Just fa))
        | f == fa =
            Left
              $ T.unpack
              $ "the following custom root field names are duplicated: "
              <> toTxt f
              <<> " and "
              <>> toTxt fa
      checkForDup fields = Right fields

instance ToJSON FunctionCustomRootFields where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

instance FromJSON FunctionCustomRootFields where
  parseJSON = withObject "Object" $ \obj -> do
    function <- obj .:? "function"
    functionAggregate <- obj .:? "function_aggregate"

    case (function, functionAggregate) of
      (Just f, Just fa)
        | f == fa ->
            fail
              $ T.unpack
              $ "the following custom root field names are duplicated: "
              <> toTxt f
              <<> " and "
              <>> toTxt fa
      _ ->
        pure ()

    pure $ FunctionCustomRootFields function functionAggregate

-- | A function custom root fields without custom names set. This is the default.
emptyFunctionCustomRootFields :: FunctionCustomRootFields
emptyFunctionCustomRootFields =
  FunctionCustomRootFields
    { _fcrfFunction = Nothing,
      _fcrfFunctionAggregate = Nothing
    }

-- | Tracked SQL function metadata. See 'buildFunctionInfo'.
data FunctionInfo (b :: BackendType) = FunctionInfo
  { _fiSQLName :: FunctionName b,
    _fiGQLName :: G.Name,
    _fiGQLArgsName :: G.Name,
    _fiGQLAggregateName :: G.Name,
    _fiSystemDefined :: SystemDefined,
    _fiVolatility :: FunctionVolatility,
    -- | In which part of the schema should this function be exposed?
    --
    -- See 'mkFunctionInfo' and '_fcExposedAs'.
    _fiExposedAs :: FunctionExposedAs,
    _fiInputArgs :: Seq.Seq (FunctionInputArgument b),
    -- | NOTE: when a table is created, a new composite type of the same name is
    -- automatically created; so strictly speaking this field means "the function
    -- returns the composite type corresponding to this table".
    _fiReturnType :: TableName b, -- NOTE: We will extend this in future, but for now always resolves to a (TableName b)

    -- | this field represents the description of the function as present on the database
    _fiDescription :: Maybe Text,
    -- | Roles to which the function is accessible
    _fiPermissions :: FunctionPermissionsMap,
    _fiJsonAggSelect :: JsonAggSelect,
    _fiComment :: Maybe Text
  }
  deriving (Generic)

deriving instance (Backend b) => Show (FunctionInfo b)

deriving instance (Backend b) => Eq (FunctionInfo b)

instance (Backend b) => ToJSON (FunctionInfo b) where
  toJSON = genericToJSON hasuraJSON

type FunctionCache b = HashMap (FunctionName b) (FunctionInfo b) -- info of all functions

data TrackableFunctionInfo b = TrackableFunctionInfo
  { tfiFunctionName :: FunctionName b,
    tfiFunctionVolitility :: FunctionVolatility
  }
  deriving (Generic)

deriving instance (Backend b) => Show (TrackableFunctionInfo b)

deriving instance (Backend b) => Eq (TrackableFunctionInfo b)

instance (Backend b) => ToJSON (TrackableFunctionInfo b) where
  toJSON (TrackableFunctionInfo name volitility) =
    object
      [ "name" Data.Aeson..= name,
        "volitility" Data.Aeson..= volitility
      ]

newtype TrackableTableInfo b = TrackableTableInfo
  {tfTableiName :: TableName b}
  deriving (Generic)

deriving instance (Backend b) => Show (TrackableTableInfo b)

deriving instance (Backend b) => Eq (TrackableTableInfo b)

instance (Backend b) => ToJSON (TrackableTableInfo b) where
  toJSON (TrackableTableInfo ti) = object ["name" Data.Aeson..= ti]

data TrackableInfo b = TrackableInfo
  { trackableFunctions :: [TrackableFunctionInfo b],
    trackableTables :: [TrackableTableInfo b]
  }
  deriving (Generic)

deriving instance (Backend b) => Show (TrackableInfo b)

deriving instance (Backend b) => Eq (TrackableInfo b)

instance (Backend b) => ToJSON (TrackableInfo b) where
  toJSON (TrackableInfo functions tables) =
    object
      [ "tables" Data.Aeson..= tables,
        "functions" Data.Aeson..= functions
      ]

-- Metadata requests related types

-- | Tracked function configuration, and payload of the 'pg_track_function' and
-- 'pg_set_function_customization' API calls.
data FunctionConfig b = FunctionConfig
  { _fcSessionArgument :: Maybe FunctionArgName,
    -- | In which top-level field should we expose this function?
    --
    -- The user might omit this, in which case we'll infer the location from the
    -- SQL functions volatility. See 'mkFunctionInfo' or the @track_function@ API
    -- docs for details of validation, etc.
    _fcExposedAs :: Maybe FunctionExposedAs,
    _fcCustomRootFields :: FunctionCustomRootFields,
    _fcCustomName :: Maybe G.Name,
    _fcResponse :: Maybe (FunctionReturnType b)
  }
  deriving (Generic)

deriving stock instance (Backend b) => Show (FunctionConfig b)

deriving stock instance (Backend b) => Eq (FunctionConfig b)

instance (Backend b) => NFData (FunctionConfig b)

instance (Backend b) => HasCodec (FunctionConfig b) where
  codec =
    AC.object "FunctionConfig"
      $ FunctionConfig
      <$> AC.optionalField' "session_argument"
      AC..= _fcSessionArgument
        <*> AC.optionalField' "exposed_as"
      AC..= _fcExposedAs
        <*> AC.optionalFieldWithDefault' "custom_root_fields" emptyFunctionCustomRootFields
      AC..= _fcCustomRootFields
        <*> AC.optionalFieldWith' "custom_name" graphQLFieldNameCodec
      AC..= _fcCustomName
        <*> AC.optionalFieldWith' "response" codec
      AC..= _fcResponse

instance (Backend b) => FromJSON (FunctionConfig b) where
  parseJSON = withObject "FunctionConfig" $ \obj ->
    FunctionConfig
      <$> obj
      .:? "session_argument"
      <*> obj
      .:? "exposed_as"
      <*> obj
      .:? "custom_root_fields"
      .!= emptyFunctionCustomRootFields
      <*> obj
      .:? "custom_name"
      <*> obj
      .:? "response"

instance (Backend b) => ToJSON (FunctionConfig b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

-- | The default function config; v1 of the API implies this.
emptyFunctionConfig :: FunctionConfig b
emptyFunctionConfig = FunctionConfig Nothing Nothing emptyFunctionCustomRootFields Nothing Nothing

type DBFunctionsMetadata b = HashMap (FunctionName b) (FunctionOverloads b)

newtype FunctionOverloads b = FunctionOverloads {getFunctionOverloads :: NonEmpty (RawFunctionInfo b)}

deriving newtype instance (Backend b) => Eq (FunctionOverloads b)

deriving newtype instance (Backend b) => Show (FunctionOverloads b)

deriving newtype instance (FromJSON (RawFunctionInfo b)) => FromJSON (FunctionOverloads b)

deriving newtype instance (ToJSON (RawFunctionInfo b)) => ToJSON (FunctionOverloads b)

data FunctionArgsExpG a = FunctionArgsExp
  { _faePositional :: [a],
    _faeNamed :: (HashMap.HashMap Text a)
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (FunctionArgsExpG a)

instance (NFData a) => NFData (FunctionArgsExpG a)

type FunctionArgsExp b v = FunctionArgsExpG (FunctionArgumentExp b v)

emptyFunctionArgsExp :: FunctionArgsExpG a
emptyFunctionArgsExp = FunctionArgsExp [] HashMap.empty
