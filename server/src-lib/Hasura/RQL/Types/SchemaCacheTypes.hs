module Hasura.RQL.Types.SchemaCacheTypes where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Hasura.Prelude

import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types

data TableObjId
  = TOCol !PGCol
  | TORel !RelName
  | TOCons !ConstraintName
  | TOPerm !RoleName !PermType
  | TOTrigger !TriggerName
  deriving (Show, Eq, Generic)

instance Hashable TableObjId

data SchemaObjId
  = SOTable !QualifiedTable
  | SOQTemplate !TQueryName
  | SOTableObj !QualifiedTable !TableObjId
  | SOFunction !QualifiedFunction
   deriving (Eq, Generic)

instance Hashable SchemaObjId

reportSchemaObj :: SchemaObjId -> T.Text
reportSchemaObj (SOTable tn) = "table " <> qualObjectToText tn
reportSchemaObj (SOFunction fn) = "function " <> qualObjectToText fn
reportSchemaObj (SOQTemplate qtn) =
  "query-template " <> getTQueryName qtn
reportSchemaObj (SOTableObj tn (TOCol cn)) =
  "column " <> qualObjectToText tn <> "." <> getPGColTxt cn
reportSchemaObj (SOTableObj tn (TORel cn)) =
  "relationship " <> qualObjectToText tn <> "." <> getRelTxt cn
reportSchemaObj (SOTableObj tn (TOCons cn)) =
  "constraint " <> qualObjectToText tn <> "." <> getConstraintTxt cn
reportSchemaObj (SOTableObj tn (TOPerm rn pt)) =
  "permission " <> qualObjectToText tn <> "." <> getRoleTxt rn
  <> "." <> permTypeToCode pt
reportSchemaObj (SOTableObj tn (TOTrigger trn )) =
  "event-trigger " <> qualObjectToText tn <> "." <> trn


instance Show SchemaObjId where
  show soi = T.unpack $ reportSchemaObj soi

instance ToJSON SchemaObjId where
  toJSON = String . reportSchemaObj

instance ToJSONKey SchemaObjId where
  toJSONKey = toJSONKeyText reportSchemaObj

data SchemaDependency
  = SchemaDependency
  { sdObjId  :: !SchemaObjId
  , sdReason :: !T.Text
  } deriving (Show, Eq, Generic)

$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaDependency)
instance Hashable SchemaDependency

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
  deriving (Show, Eq, ToJSON)

data FunctionArg
  = FunctionArg
  { faName :: !(Maybe FunctionArgName)
  , faType :: !PGColType
  } deriving(Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionArg)

data FunctionInfoG a
  = FunctionInfoG
  { fiName          :: !QualifiedFunction
  , fiSystemDefined :: !Bool
  , fiType          :: !FunctionType
  , fiInputArgs     :: !(Seq.Seq FunctionArg)
  , fiReturnType    :: !a
  , fiDeps          :: ![SchemaDependency]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionInfoG)

type QueryFunc = FunctionInfoG QualifiedTable
type CompColFunc = FunctionInfoG PGColType

data SQLFunction
   = SFQuery !QueryFunc
   | SFCompCol !QualifiedTable !CompColFunc
   deriving (Show, Eq)

$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "info"
                 }
   ''SQLFunction
 )
