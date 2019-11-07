module Hasura.RQL.Types.Function where

import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types

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
  deriving (Eq)

$(deriveJSON defaultOptions{constructorTagModifier = drop 2} ''FunctionType)

funcTypToTxt :: FunctionType -> T.Text
funcTypToTxt FTVOLATILE  = "VOLATILE"
funcTypToTxt FTIMMUTABLE = "IMMUTABLE"
funcTypToTxt FTSTABLE    = "STABLE"

instance Show FunctionType where
  show = T.unpack . funcTypToTxt

newtype FunctionArgName =
  FunctionArgName { getFuncArgNameTxt :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, Lift, DQuote, IsString)

newtype HasDefault = HasDefault { unHasDefault :: Bool }
  deriving (Show, Eq, ToJSON)

data FunctionArg
  = FunctionArg
  { faName       :: !(Maybe FunctionArgName)
  , faType       :: !QualifiedPGType
  , faHasDefault :: !HasDefault
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionArg)

newtype FunctionArgIndex = FunctionArgIndex {unFunctionArgIndex :: Int}
  deriving (Show, Eq, ToJSON)

data SessionArgument
  = SessionArgument
  { saName  :: !FunctionArgName
  , saIndex :: !FunctionArgIndex
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''SessionArgument)

data InputArguments
  = IAWithSessionArgument !SessionArgument !(Seq.Seq FunctionArg)
  | IAWithoutSessionArgument !(Seq.Seq FunctionArg)
  deriving (Show, Eq)

instance ToJSON InputArguments where
  toJSON (IAWithSessionArgument sessArg inpArgs) =
    object [ "session_argument" .= sessArg
           , "arguments" .= inpArgs
           ]
  toJSON (IAWithoutSessionArgument inpArgs) =
    object ["arguments" .= inpArgs]

data FunctionInfo
  = FunctionInfo
  { fiName          :: !QualifiedFunction
  , fiSystemDefined :: !SystemDefined
  , fiType          :: !FunctionType
  , fiInputArgs     :: !InputArguments
  , fiReturnType    :: !QualifiedTable
  , fiDescription   :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionInfo)

getInputArgs :: FunctionInfo -> Seq.Seq FunctionArg
getInputArgs functionInfo =
  case fiInputArgs functionInfo of
    IAWithSessionArgument _ args  -> args
    IAWithoutSessionArgument args -> args

getSessionArg :: FunctionInfo -> Maybe SessionArgument
getSessionArg functionInfo =
  case fiInputArgs functionInfo of
    IAWithSessionArgument sessArg _ -> Just sessArg
    IAWithoutSessionArgument _      -> Nothing
