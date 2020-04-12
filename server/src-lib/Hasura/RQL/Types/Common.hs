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

       , OID(..)
       , Constraint(..)
       , PrimaryKey(..)
       , pkConstraint
       , pkColumns
       , ForeignKey(..)
       , CustomColumnNames

       , NonEmptyText
       , mkNonEmptyTextUnsafe
       , mkNonEmptyText
       , unNonEmptyText
       , nonEmptyText
       , adminText
       , rootText

       , SystemDefined(..)
       , isSystemDefined
       ) where

import           Hasura.Incremental            (Cacheable)
import           Hasura.Prelude
import           Hasura.SQL.Types

import           Control.Lens                  (makeLenses)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift, Q, TExp)

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G
import qualified PostgreSQL.Binary.Decoding    as PD
import qualified Test.QuickCheck               as QC

newtype NonEmptyText = NonEmptyText { unNonEmptyText :: T.Text }
  deriving (Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, Lift, Q.ToPrepArg, DQuote, Generic, NFData, Cacheable)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack <$> QC.listOf1 (QC.elements alphaNumerics)

mkNonEmptyText :: T.Text -> Maybe NonEmptyText
mkNonEmptyText ""   = Nothing
mkNonEmptyText text = Just $ NonEmptyText text

mkNonEmptyTextUnsafe :: T.Text -> NonEmptyText
mkNonEmptyTextUnsafe = NonEmptyText

parseNonEmptyText :: MonadFail m => Text -> m NonEmptyText
parseNonEmptyText text = case mkNonEmptyText text of
  Nothing     -> fail "empty string not allowed"
  Just neText -> return neText

nonEmptyText :: Text -> Q (TExp NonEmptyText)
nonEmptyText = parseNonEmptyText >=> \text -> [|| text ||]

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
  = RelName { getRelTxt :: NonEmptyText }
  deriving (Show, Eq, Hashable, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift, Generic, Arbitrary, NFData, Cacheable)

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
  deriving (Show, Eq, Lift, Generic)
instance NFData RelType
instance Hashable RelType
instance Cacheable RelType

instance ToJSON RelType where
  toJSON = String . relTypeToTxt

instance FromJSON RelType where
  parseJSON (String "object") = return ObjRel
  parseJSON (String "array")  = return ArrRel
  parseJSON _                 = fail "expecting either 'object' or 'array' for rel_type"

instance Q.FromCol RelType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "object" -> Just ObjRel
    "array"  -> Just ArrRel
    _   -> Nothing

data RelInfo
  = RelInfo
  { riName     :: !RelName
  , riType     :: !RelType
  , riMapping  :: !(HashMap PGCol PGCol)
  , riRTable   :: !QualifiedTable
  , riIsManual :: !Bool
  } deriving (Show, Eq, Generic)
instance NFData RelInfo
instance Cacheable RelInfo
$(deriveToJSON (aesonDrop 2 snakeCase) ''RelInfo)

newtype FieldName
  = FieldName { getFieldNameTxt :: T.Text }
  deriving ( Show, Eq, Ord, Hashable, FromJSON, ToJSON
           , FromJSONKey, ToJSONKey, Lift, Data, Generic
           , IsString, Arbitrary, NFData, Cacheable
           )

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

-- | Postgres OIDs. <https://www.postgresql.org/docs/12/datatype-oid.html>
newtype OID = OID { unOID :: Int }
  deriving (Show, Eq, NFData, Hashable, ToJSON, FromJSON, Q.FromCol, Cacheable)

data Constraint
  = Constraint
  { _cName :: !ConstraintName
  , _cOid  :: !OID
  } deriving (Show, Eq, Generic)
instance NFData Constraint
instance Hashable Constraint
instance Cacheable Constraint
$(deriveJSON (aesonDrop 2 snakeCase) ''Constraint)

data PrimaryKey a
  = PrimaryKey
  { _pkConstraint :: !Constraint
  , _pkColumns    :: !(NonEmpty a)
  } deriving (Show, Eq, Generic)
instance (NFData a) => NFData (PrimaryKey a)
instance (Cacheable a) => Cacheable (PrimaryKey a)
$(makeLenses ''PrimaryKey)
$(deriveJSON (aesonDrop 3 snakeCase) ''PrimaryKey)

data ForeignKey
  = ForeignKey
  { _fkConstraint    :: !Constraint
  , _fkForeignTable  :: !QualifiedTable
  , _fkColumnMapping :: !ColMapping
  } deriving (Show, Eq, Generic)
instance NFData ForeignKey
instance Hashable ForeignKey
instance Cacheable ForeignKey
$(deriveJSON (aesonDrop 3 snakeCase) ''ForeignKey)

type CustomColumnNames = HM.HashMap PGCol G.Name

newtype SystemDefined = SystemDefined { unSystemDefined :: Bool }
  deriving (Show, Eq, FromJSON, ToJSON, Q.ToPrepArg, NFData, Cacheable)

isSystemDefined :: SystemDefined -> Bool
isSystemDefined = unSystemDefined
