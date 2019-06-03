module Hasura.RQL.Types.Common
       ( PGColInfo(..)
       , RelName(..)
       , RelType(..)
       , relTypeToTxt
       , RelInfo(..)

       , FieldName(..)
       , fromPGCol
       , fromRel

       , TQueryName(..)
       , TemplateParam(..)

       , ToAesonPairs(..)
       , WithTable(..)
       , ColVals
       , MutateResp(..)
       , ForeignKey(..)
       ) where

import           Hasura.Prelude
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import qualified PostgreSQL.Binary.Decoding as PD

data PGColInfo
  = PGColInfo
  { pgiName       :: !PGCol
  , pgiType       :: !PGColType
  , pgiIsNullable :: !Bool
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase) ''PGColInfo)

newtype RelName
  = RelName {getRelTxt :: T.Text}
  deriving (Show, Eq, Hashable, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift)

instance IsIden RelName where
  toIden (RelName r) = Iden r

instance DQuote RelName where
  dquoteTxt (RelName r) = r

relTypeToTxt :: RelType -> T.Text
relTypeToTxt ObjRel = "object"
relTypeToTxt ArrRel = "array"

data RelType
  = ObjRel
  | ArrRel
  deriving (Show, Eq, Generic)

instance Hashable RelType

instance ToJSON RelType where
  toJSON = String . relTypeToTxt

instance FromJSON RelType where
  parseJSON (String "object") = return ObjRel
  parseJSON (String "array") = return ArrRel
  parseJSON _ = fail "expecting either 'object' or 'array' for rel_type"

instance Q.FromCol RelType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "object" -> Just ObjRel
    "array"  -> Just ArrRel
    _   -> Nothing

data RelInfo
  = RelInfo
  { riName     :: !RelName
  , riType     :: !RelType
  , riMapping  :: ![(PGCol, PGCol)]
  , riRTable   :: !QualifiedTable
  , riIsManual :: !Bool
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''RelInfo)

newtype FieldName
  = FieldName { getFieldNameTxt :: T.Text }
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Lift)

instance IsIden FieldName where
  toIden (FieldName f) = Iden f

instance DQuote FieldName where
  dquoteTxt (FieldName c) = c

fromPGCol :: PGCol -> FieldName
fromPGCol (PGCol c) = FieldName c

fromRel :: RelName -> FieldName
fromRel (RelName r) = FieldName r

newtype TQueryName
  = TQueryName { getTQueryName :: T.Text }
  deriving ( Show, Eq, Hashable, FromJSONKey, ToJSONKey
           , FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift)

instance IsIden TQueryName where
  toIden (TQueryName r) = Iden r

instance DQuote TQueryName where
  dquoteTxt (TQueryName r) = r

newtype TemplateParam
  = TemplateParam { getTemplateParam :: T.Text }
  deriving (Show, Eq, Hashable, FromJSON, FromJSONKey, ToJSONKey, ToJSON, Lift)

instance DQuote TemplateParam where
  dquoteTxt (TemplateParam r) = r

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data WithTable a
  = WithTable
  { wtName :: !QualifiedTable
  , wtInfo :: !a
  } deriving (Show, Eq, Lift)

instance (FromJSON a) => FromJSON (WithTable a) where
  parseJSON v@(Object o) =
    WithTable <$> o .: "table" <*> parseJSON v
  parseJSON _ =
    fail "expecting an Object with key 'table'"

instance (ToAesonPairs a) => ToJSON (WithTable a) where
  toJSON (WithTable tn rel) =
    object $ ("table" .= tn):toAesonPairs rel

type ColVals = HM.HashMap PGCol Value

data MutateResp
  = MutateResp
  { _mrAffectedRows     :: !Int
  , _mrReturningColumns :: ![ColVals]
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''MutateResp)

type ColMapping = HM.HashMap PGCol PGCol

data ForeignKey
  = ForeignKey
  { _fkTable         :: !QualifiedTable
  , _fkRefTable      :: !QualifiedTable
  , _fkOid           :: !Int
  , _fkConstraint    :: !ConstraintName
  , _fkColumnMapping :: !ColMapping
  } deriving (Show, Eq, Generic)
$(deriveJSON (aesonDrop 3 snakeCase) ''ForeignKey)

instance Hashable ForeignKey
