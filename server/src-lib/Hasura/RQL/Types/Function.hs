module Hasura.RQL.Types.Function where

import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax        (Lift)

import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T

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
  deriving (Show, Eq, ToJSON, FromJSON, Lift, DQuote)

data FunctionArg
  = FunctionArg
  { faName       :: !(Maybe FunctionArgName)
  , faType       :: !PGScalarType
  , faHasDefault :: !Bool
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionArg)

data SessionArgument
  = SessionArgument
  { saName  :: !FunctionArgName
  , saIndex :: !Int
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''SessionArgument)

data FunctionInfo
  = FunctionInfo
  { fiName          :: !QualifiedFunction
  , fiSystemDefined :: !SystemDefined
  , fiType          :: !FunctionType
  , fiInputArgs     :: !(Seq.Seq FunctionArg)
  , fiSessionVarArg :: !(Maybe SessionArgument)
  , fiReturnType    :: !QualifiedTable
  , fiDeps          :: ![SchemaDependency]
  , fiDescription   :: !(Maybe PGDescription)
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionInfo)
