module Hasura.RQL.Types.Common
       ( RelName(..)
       , relNameToTxt
       , RelType(..)
       , rootRelName
       , relTypeToTxt
       , RelInfo(..)

       , FieldName(..)
       , fromPGCol
       , fromRel

       , ToAesonPairs(..)
       , WithTable(..)
       , ColumnValues
       , MutateResp(..)
       , ForeignKey(..)
       , CustomColumnNames

       , NonEmptyText
       , mkNonEmptyText
       , unNonEmptyText
       , adminText
       , rootText

       , SystemDefined(..)
       , isSystemDefined
       ) where

import           Hasura.Prelude
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G
import qualified PostgreSQL.Binary.Decoding    as PD
import qualified Test.QuickCheck               as QC

newtype NonEmptyText = NonEmptyText {unNonEmptyText :: T.Text}
  deriving (Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, Lift, Q.ToPrepArg, DQuote, Generic)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack <$> QC.listOf1 (QC.elements alphaNumerics)

mkNonEmptyText :: T.Text -> Maybe NonEmptyText
mkNonEmptyText ""   = Nothing
mkNonEmptyText text = Just $ NonEmptyText text

parseNonEmptyText :: T.Text -> Parser NonEmptyText
parseNonEmptyText text = case mkNonEmptyText text of
  Nothing     -> fail "empty string not allowed"
  Just neText -> return neText

instance FromJSON NonEmptyText where
  parseJSON = withText "String" parseNonEmptyText

instance FromJSONKey NonEmptyText where
  fromJSONKey = FromJSONKeyTextParser parseNonEmptyText

instance Q.FromCol NonEmptyText where
  fromCol bs = mkNonEmptyText <$> Q.fromCol bs
    >>= maybe (Left "empty string not allowed") Right

adminText :: NonEmptyText
adminText = NonEmptyText "admin"

rootText :: NonEmptyText
rootText = NonEmptyText "root"

newtype RelName
  = RelName {getRelTxt :: NonEmptyText}
  deriving (Show, Eq, Hashable, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift, Generic, Arbitrary)

instance IsIden RelName where
  toIden rn = Iden $ relNameToTxt rn

instance DQuote RelName where
  dquoteTxt = relNameToTxt

rootRelName :: RelName
rootRelName = RelName rootText

relNameToTxt :: RelName -> T.Text
relNameToTxt = unNonEmptyText . getRelTxt

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
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Lift, Data, Generic, Arbitrary)

instance IsIden FieldName where
  toIden (FieldName f) = Iden f

instance DQuote FieldName where
  dquoteTxt (FieldName c) = c

fromPGCol :: PGCol -> FieldName
fromPGCol c = FieldName $ getPGColTxt c

fromRel :: RelName -> FieldName
fromRel = FieldName . relNameToTxt

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

type ColumnValues a = HM.HashMap PGCol a

data MutateResp a
  = MutateResp
  { _mrAffectedRows     :: !Int
  , _mrReturningColumns :: ![ColumnValues a]
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

type CustomColumnNames = HM.HashMap PGCol G.Name

newtype SystemDefined = SystemDefined { unSystemDefined :: Bool }
  deriving (Show, Eq, FromJSON, ToJSON, Q.ToPrepArg)

isSystemDefined :: SystemDefined -> Bool
isSystemDefined = unSystemDefined
