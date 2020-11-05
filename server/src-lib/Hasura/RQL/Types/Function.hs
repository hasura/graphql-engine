module Hasura.RQL.Types.Function where

import           Hasura.Prelude

import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended
import           Language.Haskell.TH.Syntax         (Lift)

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Types.Common


data FunctionType
  = FTVOLATILE
  | FTIMMUTABLE
  | FTSTABLE
  deriving (Eq, Generic)
instance NFData FunctionType
instance Cacheable FunctionType
$(deriveJSON defaultOptions{constructorTagModifier = drop 2} ''FunctionType)

funcTypToTxt :: FunctionType -> Text
funcTypToTxt FTVOLATILE  = "VOLATILE"
funcTypToTxt FTIMMUTABLE = "IMMUTABLE"
funcTypToTxt FTSTABLE    = "STABLE"

instance Show FunctionType where
  show = T.unpack . funcTypToTxt

newtype FunctionArgName =
  FunctionArgName { getFuncArgNameTxt :: Text}
  deriving (Show, Eq, NFData, ToJSON, FromJSON, Lift, ToTxt, IsString, Generic, Arbitrary, Cacheable)

newtype HasDefault = HasDefault { unHasDefault :: Bool }
  deriving (Show, Eq, ToJSON, Cacheable)

data FunctionArg
  = FunctionArg
  { faName       :: !(Maybe FunctionArgName)
  , faType       :: !QualifiedPGType
  , faHasDefault :: !HasDefault
  } deriving (Show, Eq, Generic)
instance Cacheable FunctionArg
$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionArg)

data InputArgument a
  = IAUserProvided !a
  | IASessionVariables !FunctionArgName
  deriving (Show, Eq, Functor)
$(deriveToJSON defaultOptions
               { constructorTagModifier = snakeCase . drop 2
               , sumEncoding = TaggedObject "type" "argument"
               }
 ''InputArgument
 )
$(makePrisms ''InputArgument)

type FunctionInputArgument = InputArgument FunctionArg

data FunctionInfo
  = FunctionInfo
  { fiName          :: !QualifiedFunction
  , fiSystemDefined :: !SystemDefined
  , fiType          :: !FunctionType
  , fiInputArgs     :: !(Seq.Seq FunctionInputArgument)
  , fiReturnType    :: !QualifiedTable
  , fiDescription   :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionInfo)

getInputArgs :: FunctionInfo -> Seq.Seq FunctionArg
getInputArgs =
  Seq.fromList . mapMaybe (^? _IAUserProvided) . toList . fiInputArgs

type FunctionCache = HashMap QualifiedFunction FunctionInfo -- info of all functions

-- Metadata requests related types
data FunctionConfig
  = FunctionConfig
  { _fcSessionArgument :: !(Maybe FunctionArgName)
  } deriving (Show, Eq, Generic, Lift)
instance NFData FunctionConfig
instance Cacheable FunctionConfig
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields = True} ''FunctionConfig)

emptyFunctionConfig :: FunctionConfig
emptyFunctionConfig = FunctionConfig Nothing

data TrackFunctionV2
  = TrackFunctionV2
  { _tfv2Function      :: !QualifiedFunction
  , _tfv2Configuration :: !FunctionConfig
  } deriving (Show, Eq, Lift, Generic)
$(deriveToJSON (aesonDrop 5 snakeCase) ''TrackFunctionV2)

instance FromJSON TrackFunctionV2 where
  parseJSON = withObject "Object" $ \o ->
    TrackFunctionV2
    <$> o .: "function"
    <*> o .:? "configuration" .!= emptyFunctionConfig

-- | Raw SQL function metadata from postgres
data RawFunctionInfo
  = RawFunctionInfo
  { rfiHasVariadic      :: !Bool
  , rfiFunctionType     :: !FunctionType
  , rfiReturnTypeSchema :: !SchemaName
  , rfiReturnTypeName   :: !PGScalarType
  , rfiReturnTypeType   :: !PGTypeKind
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![QualifiedPGType]
  , rfiInputArgNames    :: ![FunctionArgName]
  , rfiDefaultArgs      :: !Int
  , rfiReturnsTable     :: !Bool
  , rfiDescription      :: !(Maybe PGDescription)
  } deriving (Show, Eq, Generic)
instance NFData RawFunctionInfo
instance Cacheable RawFunctionInfo
$(deriveJSON (aesonDrop 3 snakeCase) ''RawFunctionInfo)

type PostgresFunctionsMetadata = HashMap QualifiedFunction [RawFunctionInfo]
