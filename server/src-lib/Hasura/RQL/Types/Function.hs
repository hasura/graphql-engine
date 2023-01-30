{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Function
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
    emptyFunctionConfig,
    emptyFunctionCustomRootFields,
    fiComment,
    fiDescription,
    fiExposedAs,
    fiGQLAggregateName,
    fiGQLArgsName,
    fiGQLName,
    fiInputArgs,
    fiJsonAggSelect,
    fiPermissions,
    fiReturnType,
    fiSQLName,
    fiSystemDefined,
    fiVolatility,
    fpmRole,
    funcTypToTxt,
    getFunctionAggregateGQLName,
    getFunctionArgsGQLName,
    getFunctionGQLName,
    getInputArgs,
    emptyFunctionArgsExp,
    _IASessionVariables,
    _IAUserProvided,
  )
where

import Autodocodec
  ( HasCodec (codec),
    bimapCodec,
    dimapCodec,
    optionalField',
    optionalFieldWith',
    optionalFieldWithDefault',
    requiredField',
    stringConstCodec,
  )
import Autodocodec qualified as AC
import Autodocodec.Extended (graphQLFieldNameCodec)
import Control.Lens
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Char (toLower)
import Data.HashMap.Strict qualified as HM
import Data.List.Extended as LE
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax

-- | https://www.postgresql.org/docs/current/xfunc-volatility.html
data FunctionVolatility
  = FTVOLATILE
  | FTIMMUTABLE
  | FTSTABLE
  deriving (Eq, Generic)

instance NFData FunctionVolatility

$(deriveJSON defaultOptions {constructorTagModifier = drop 2} ''FunctionVolatility)

funcTypToTxt :: FunctionVolatility -> Text
funcTypToTxt FTVOLATILE = "VOLATILE"
funcTypToTxt FTIMMUTABLE = "IMMUTABLE"
funcTypToTxt FTSTABLE = "STABLE"

instance Show FunctionVolatility where
  show = T.unpack . funcTypToTxt

newtype FunctionArgName = FunctionArgName {getFuncArgNameTxt :: Text}
  deriving (Show, Eq, Ord, NFData, ToJSON, ToJSONKey, FromJSON, FromJSONKey, ToTxt, IsString, Generic, Hashable, Lift, Data)

instance HasCodec FunctionArgName where
  codec = dimapCodec FunctionArgName getFuncArgNameTxt codec

data InputArgument a
  = IAUserProvided a
  | IASessionVariables FunctionArgName
  deriving (Show, Eq, Functor)

$( deriveToJSON
     defaultOptions
       { constructorTagModifier = snakeCase . drop 2,
         sumEncoding = TaggedObject "type" "argument"
       }
     ''InputArgument
 )
$(makePrisms ''InputArgument)

type FunctionInputArgument b = InputArgument (FunctionArgument b)

-- | Indicates whether the user requested the corresponding function to be
-- tracked as a mutation or a query/subscription, in @track_function@.
data FunctionExposedAs = FEAQuery | FEAMutation
  deriving (Show, Eq, Generic)

instance NFData FunctionExposedAs

instance HasCodec FunctionExposedAs where
  codec = stringConstCodec [(FEAQuery, "query"), (FEAMutation, "mutation")]

$( deriveJSON
     defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 3}
     ''FunctionExposedAs
 )

newtype FunctionPermissionInfo = FunctionPermissionInfo
  { _fpmRole :: RoleName
  }
  deriving (Show, Eq, Generic)

instance HasCodec FunctionPermissionInfo where
  codec =
    AC.object "FunctionPermissionInfo" $
      FunctionPermissionInfo <$> requiredField' "role" AC..= _fpmRole

$(makeLenses ''FunctionPermissionInfo)
$(deriveJSON hasuraJSON ''FunctionPermissionInfo)

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
    bimapCodec checkForDup id $
      AC.object "FunctionCustomRootFields" $
        FunctionCustomRootFields
          <$> optionalFieldWith' "function" graphQLFieldNameCodec AC..= _fcrfFunction
          <*> optionalFieldWith' "function_aggregate" graphQLFieldNameCodec AC..= _fcrfFunctionAggregate
    where
      checkForDup (FunctionCustomRootFields (Just f) (Just fa))
        | f == fa =
            Left $
              T.unpack $
                "the following custom root field names are duplicated: " <> toTxt f <<> " and " <>> toTxt fa
      checkForDup fields = Right fields

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''FunctionCustomRootFields)

instance FromJSON FunctionCustomRootFields where
  parseJSON = withObject "Object" $ \obj -> do
    function <- obj .:? "function"
    functionAggregate <- obj .:? "function_aggregate"

    case (function, functionAggregate) of
      (Just f, Just fa)
        | f == fa ->
            fail $
              T.unpack $
                "the following custom root field names are duplicated: "
                  <> toTxt f <<> " and " <>> toTxt fa
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
    _fiReturnType :: TableName b,
    -- | this field represents the description of the function as present on the database
    _fiDescription :: Maybe Text,
    -- | Roles to which the function is accessible
    _fiPermissions :: FunctionPermissionsMap,
    _fiJsonAggSelect :: JsonAggSelect,
    _fiComment :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Show (FunctionInfo b)

deriving instance Backend b => Eq (FunctionInfo b)

instance (Backend b) => ToJSON (FunctionInfo b) where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''FunctionInfo)

-- | Apply function name customization to function arguments, as detailed in
-- 'rfcs/function-root-field-customisation.md'.  We want the different
-- variations of a function (i.e. basic, aggregate) to share the same type name
-- for their arguments.
getFunctionArgsGQLName ::
  -- | The GQL version of the DB name of the function
  G.Name ->
  FunctionConfig ->
  -- | Custom function for setting naming case
  (G.Name -> G.Name) ->
  G.Name
getFunctionArgsGQLName
  funcGivenName
  FunctionConfig {..}
  setCase =
    setCase $ fromMaybe funcGivenName _fcCustomName <> Name.__args

-- | Apply function name customization to the basic function variation, as
-- detailed in 'rfcs/function-root-field-customisation.md'.
getFunctionGQLName ::
  G.Name ->
  FunctionConfig ->
  -- | Custom function for setting naming case
  (G.Name -> G.Name) ->
  G.Name
getFunctionGQLName
  funcGivenName
  FunctionConfig
    { _fcCustomRootFields = FunctionCustomRootFields {..},
      ..
    }
  setCase =
    choice
      [ _fcrfFunction,
        _fcCustomName
      ]
      & fromMaybe (setCase funcGivenName)

-- | Apply function name customization to the aggregate function variation, as
-- detailed in 'rfcs/function-root-field-customisation.md'.
getFunctionAggregateGQLName ::
  G.Name ->
  FunctionConfig ->
  -- | Custom function for setting naming case
  (G.Name -> G.Name) ->
  G.Name
getFunctionAggregateGQLName
  funcGivenName
  FunctionConfig
    { _fcCustomRootFields = FunctionCustomRootFields {..},
      ..
    }
  setCase =
    choice
      [ _fcrfFunctionAggregate,
        _fcCustomName <&> (<> Name.__aggregate)
      ]
      & fromMaybe (setCase $ funcGivenName <> Name.__aggregate)

getInputArgs :: FunctionInfo b -> Seq.Seq (FunctionArgument b)
getInputArgs =
  Seq.fromList . mapMaybe (^? _IAUserProvided) . toList . _fiInputArgs

type FunctionCache b = HashMap (FunctionName b) (FunctionInfo b) -- info of all functions

-- Metadata requests related types

-- | Tracked function configuration, and payload of the 'pg_track_function' and
-- 'pg_set_function_customization' API calls.
data FunctionConfig = FunctionConfig
  { _fcSessionArgument :: Maybe FunctionArgName,
    -- | In which top-level field should we expose this function?
    --
    -- The user might omit this, in which case we'll infer the location from the
    -- SQL functions volatility. See 'mkFunctionInfo' or the @track_function@ API
    -- docs for details of validation, etc.
    _fcExposedAs :: Maybe FunctionExposedAs,
    _fcCustomRootFields :: FunctionCustomRootFields,
    _fcCustomName :: Maybe G.Name
  }
  deriving (Show, Eq, Generic)

instance NFData FunctionConfig

instance HasCodec FunctionConfig where
  codec =
    AC.object "FunctionConfig" $
      FunctionConfig
        <$> optionalField' "session_argument" AC..= _fcSessionArgument
        <*> optionalField' "exposed_as" AC..= _fcExposedAs
        <*> optionalFieldWithDefault' "custom_root_fields" emptyFunctionCustomRootFields AC..= _fcCustomRootFields
        <*> optionalFieldWith' "custom_name" graphQLFieldNameCodec AC..= _fcCustomName

instance FromJSON FunctionConfig where
  parseJSON = withObject "FunctionConfig" $ \obj ->
    FunctionConfig
      <$> obj .:? "session_argument"
      <*> obj .:? "exposed_as"
      <*> obj .:? "custom_root_fields" .!= emptyFunctionCustomRootFields
      <*> obj .:? "custom_name"

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''FunctionConfig)

-- | The default function config; v1 of the API implies this.
emptyFunctionConfig :: FunctionConfig
emptyFunctionConfig = FunctionConfig Nothing Nothing emptyFunctionCustomRootFields Nothing

type DBFunctionsMetadata b = HashMap (FunctionName b) (FunctionOverloads b)

newtype FunctionOverloads b = FunctionOverloads {getFunctionOverloads :: NonEmpty (RawFunctionInfo b)}

deriving newtype instance Backend b => Eq (FunctionOverloads b)

deriving newtype instance FromJSON (RawFunctionInfo b) => FromJSON (FunctionOverloads b)

data FunctionArgsExpG a = FunctionArgsExp
  { _faePositional :: [a],
    _faeNamed :: (HM.HashMap Text a)
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (FunctionArgsExpG a)

instance (NFData a) => NFData (FunctionArgsExpG a)

type FunctionArgsExp b v = FunctionArgsExpG (FunctionArgumentExp b v)

emptyFunctionArgsExp :: FunctionArgsExpG a
emptyFunctionArgsExp = FunctionArgsExp [] HM.empty
