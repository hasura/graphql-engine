module Hasura.RQL.Types.Function where

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Sequence              as Seq
import qualified Data.Text                  as T

data FunctionType
  = FTVOLATILE
  | FTIMMUTABLE
  | FTSTABLE
  deriving (Eq, Generic)
instance NFData FunctionType
instance Cacheable FunctionType
$(deriveJSON defaultOptions{constructorTagModifier = drop 2} ''FunctionType)

funcTypToTxt :: FunctionType -> T.Text
funcTypToTxt FTVOLATILE  = "VOLATILE"
funcTypToTxt FTIMMUTABLE = "IMMUTABLE"
funcTypToTxt FTSTABLE    = "STABLE"

instance Show FunctionType where
  show = T.unpack . funcTypToTxt

newtype FunctionArgName =
  FunctionArgName { getFuncArgNameTxt :: T.Text}
  deriving (Show, Eq, NFData, ToJSON, FromJSON, Lift, DQuote, IsString, Generic, Arbitrary, Cacheable)

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
  { _tfv2Source        :: !SourceName
  , _tfv2Function      :: !QualifiedFunction
  , _tfv2Configuration :: !FunctionConfig
  } deriving (Show, Eq, Lift, Generic)
$(deriveToJSON (aesonDrop 5 snakeCase) ''TrackFunctionV2)

instance FromJSON TrackFunctionV2 where
  parseJSON = withObject "Object" $ \o ->
    TrackFunctionV2
    <$> o .:? "source" .!= defaultSource
    <*> o .: "function"
    <*> o .:? "configuration" .!= emptyFunctionConfig
