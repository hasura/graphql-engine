{-# LANGUAGE AllowAmbiguousTypes #-}

module Hasura.RQL.Types.Common
       ( RelName(..)
       , relNameToTxt
       , RelType(..)
       , rootRelName
       , relTypeToTxt
       , RelInfo(..)

       , Backend (..)
       , SessionVarType

       , FieldName(..)
       , fromCol
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
       , EquatableGType(..)
       , InpValInfo(..)
       , CustomColumnNames

       , adminText
       , rootText

       , SystemDefined(..)
       , isSystemDefined

       , SQLGenCtx(..)

       , successMsg
       , NonNegativeDiffTime
       , unNonNegativeDiffTime
       , unsafeNonNegativeDiffTime
       , mkNonNegativeDiffTime
       , InputWebhook(..)
       , ResolvedWebhook(..)
       , resolveWebhook
       , NonNegativeInt
       , getNonNegativeInt
       , mkNonNegativeInt
       , unsafeNonNegativeInt
       , Timeout(..)
       , defaultActionTimeoutSecs

       , UrlConf(..)
       , resolveUrlConf
       , getEnv

       , SourceName(..)
       , defaultSource
       , sourceNameToText
       ) where

import           Hasura.Prelude

import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as HM
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Language.Haskell.TH.Syntax             as TH
import qualified PostgreSQL.Binary.Decoding             as PD
import qualified Test.QuickCheck                        as QC

import           Control.Lens                           (makeLenses)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Bifunctor                         (bimap)
import           Data.Kind                              (Type)
import           Data.Scientific                        (toBoundedInteger)
import           Data.Text.Extended
import           Data.Text.NonEmpty
import           Data.Typeable
import           Data.URL.Template

import qualified Hasura.Backends.Postgres.Execute.Types as PG
import qualified Hasura.Backends.Postgres.SQL.DML       as PG
import qualified Hasura.Backends.Postgres.SQL.Types     as PG
import qualified Hasura.Backends.Postgres.SQL.Value     as PG

import           Hasura.EncJSON
import           Hasura.Incremental                     (Cacheable)
import           Hasura.RQL.DDL.Headers                 ()
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.SQL.Types

type Representable a = (Show a, Eq a, Hashable a, Cacheable a, NFData a)

-- | Mapping from abstract types to concrete backend representation
--
-- The RQL IR, used as the output of GraphQL parsers and of the RQL parsers, is
-- backend-agnostic: it uses an abstract representation of the structure of a
-- query, and delegates to the backends the task of choosing an appropriate
-- concrete representation.
--
-- Additionally, grouping all those types under one typeclass rather than having
-- dedicated type families allows to explicitly list all typeclass requirements,
-- which simplifies the instance declarations of all IR types.
class
  ( Representable (Identifier b)
  , Representable (TableName b)
  , Representable (FunctionName b)
  , Representable (ConstraintName b)
  , Representable (BasicOrderType b)
  , Representable (NullsOrderType b)
  , Representable (Column b)
  , Representable (ScalarType b)
  , Representable (SQLExpression b)
  , Representable (SQLOperator b)
  -- TODO define sufficiently many synonyms to get rid of these 4 lines for SessionVarType:
  , Eq (SessionVarType b)
  , Data (SessionVarType b)
  , NFData (SessionVarType b)
  , Cacheable (SessionVarType b)
  , Representable (XAILIKE b)
  , Representable (XANILIKE b)
  , Representable (XComputedFieldInfo b)
  , Ord (TableName b)
  , Ord (ScalarType b)
  , Data (TableName b)
  , Data (ScalarType b)
  , Data (SQLExpression b)
  , FromJSON (TableName b)
  , FromJSON (ScalarType b)
  , FromJSON (BasicOrderType b)
  , FromJSON (NullsOrderType b)
  , FromJSON (Column b)
  , ToJSON (TableName b)
  , ToJSON (ScalarType b)
  , ToJSON (BasicOrderType b)
  , ToJSON (NullsOrderType b)
  , ToJSON (Column b)
  , FromJSONKey (Column b)
  , ToJSONKey (Column b)
  , ToTxt (TableName b)
  , ToTxt (ScalarType b)
  , ToTxt (Column b)
  , Typeable b
  ) => Backend (b :: BackendType) where
  type Identifier     b :: Type
  type Alias          b :: Type
  type TableName      b :: Type
  type FunctionName   b :: Type
  type ConstraintName b :: Type
  type BasicOrderType b :: Type
  type NullsOrderType b :: Type
  type CountType      b :: Type
  type Column         b :: Type
  type ScalarValue    b :: Type
  type ScalarType     b :: Type
  type SQLExpression  b :: Type
  type SQLOperator    b :: Type
  type XAILIKE        b :: Type
  type XANILIKE       b :: Type
  type XComputedFieldInfo b :: Type
  type SourceConfig   b :: Type
  isComparableType :: ScalarType b -> Bool
  isNumType :: ScalarType b -> Bool

instance Backend 'Postgres where
  type Identifier     'Postgres = PG.Identifier
  type Alias          'Postgres = PG.Alias
  type TableName      'Postgres = PG.QualifiedTable
  type FunctionName   'Postgres = PG.QualifiedFunction
  type ConstraintName 'Postgres = PG.ConstraintName
  type BasicOrderType 'Postgres = PG.OrderType
  type NullsOrderType 'Postgres = PG.NullsOrder
  type CountType      'Postgres = PG.CountType
  type Column         'Postgres = PG.PGCol
  type ScalarValue    'Postgres = PG.PGScalarValue
  type ScalarType     'Postgres = PG.PGScalarType
  type SQLExpression  'Postgres = PG.SQLExp
  type SQLOperator    'Postgres = PG.SQLOp
  type XAILIKE        'Postgres = ()
  type XANILIKE       'Postgres = ()
  type XComputedFieldInfo 'Postgres = ()
  type SourceConfig   'Postgres = PG.PGSourceConfig
  isComparableType = PG.isComparableType
  isNumType = PG.isNumType

-- instance Backend 'Mysql where
--   type XAILIKE 'MySQL = Void
--   type XANILIKE 'MySQL = Void

type SessionVarType b = CollectableType (ScalarType b)

adminText :: NonEmptyText
adminText = mkNonEmptyTextUnsafe "admin"

rootText :: NonEmptyText
rootText = mkNonEmptyTextUnsafe "root"

newtype RelName
  = RelName { getRelTxt :: NonEmptyText }
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, ToJSONKey
           , Q.ToPrepArg, Q.FromCol, Generic, Arbitrary, NFData, Cacheable)

instance PG.IsIdentifier RelName where
  toIdentifier rn = PG.Identifier $ relNameToTxt rn

instance ToTxt RelName where
  toTxt = relNameToTxt

rootRelName :: RelName
rootRelName = RelName rootText

relNameToTxt :: RelName -> Text
relNameToTxt = unNonEmptyText . getRelTxt

relTypeToTxt :: RelType -> Text
relTypeToTxt ObjRel = "object"
relTypeToTxt ArrRel = "array"

data RelType
  = ObjRel
  | ArrRel
  deriving (Show, Eq, Generic)
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
    _        -> Nothing

-- should this be parameterized by both the source and the destination backend?
data RelInfo (b :: BackendType)
  = RelInfo
  { riName       :: !RelName
  , riType       :: !RelType
  , riMapping    :: !(HashMap (Column b) (Column b))
  , riRTable     :: !(TableName b)
  , riIsManual   :: !Bool
  , riIsNullable :: !Bool
  } deriving (Generic)
deriving instance Backend b => Show (RelInfo b)
deriving instance Backend b => Eq   (RelInfo b)
instance Backend b => NFData (RelInfo b)
instance Backend b => Cacheable (RelInfo b)
instance Backend b => Hashable (RelInfo b)

instance (Backend b) => FromJSON (RelInfo b) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance (Backend b) => ToJSON (RelInfo b) where
  toJSON = genericToJSON $ aesonPrefix snakeCase


newtype FieldName
  = FieldName { getFieldNameTxt :: Text }
  deriving ( Show, Eq, Ord, Hashable, FromJSON, ToJSON
           , FromJSONKey, ToJSONKey, Data, Generic
           , IsString, Arbitrary, NFData, Cacheable
           , Semigroup
           )

instance PG.IsIdentifier FieldName where
  toIdentifier (FieldName f) = PG.Identifier f

instance ToTxt FieldName where
  toTxt (FieldName c) = c

fromCol :: Backend b => Column b -> FieldName
fromCol = FieldName . toTxt

fromRel :: RelName -> FieldName
fromRel = FieldName . relNameToTxt

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data SourceName
  = SNDefault
  | SNName !NonEmptyText
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SourceName where
  parseJSON = withText "String" $ \case
    "default" -> pure SNDefault
    t         -> SNName <$> parseJSON (String t)

sourceNameToText :: SourceName -> Text
sourceNameToText = \case
  SNDefault -> "default"
  SNName t  -> unNonEmptyText t

instance ToJSON SourceName where
  toJSON = String . sourceNameToText

instance ToTxt SourceName where
  toTxt = sourceNameToText

instance ToJSONKey SourceName
instance Hashable SourceName
instance NFData SourceName
instance Cacheable SourceName

instance Arbitrary SourceName where
  arbitrary = SNName <$> arbitrary

defaultSource :: SourceName
defaultSource = SNDefault

data WithTable a
  = WithTable
  { wtSource :: !SourceName
  , wtName   :: !PG.QualifiedTable
  , wtInfo   :: !a
  } deriving (Show, Eq)

instance (FromJSON a) => FromJSON (WithTable a) where
  parseJSON v@(Object o) =
    WithTable
    <$> o .:? "source" .!= defaultSource
    <*> o .: "table"
    <*> parseJSON v
  parseJSON _ =
    fail "expecting an Object with key 'table'"

instance (ToAesonPairs a) => ToJSON (WithTable a) where
  toJSON (WithTable sourceName tn rel) =
    object $ ("source" .= sourceName):("table" .= tn):toAesonPairs rel

type ColumnValues a = HM.HashMap PG.PGCol a

data MutateResp a
  = MutateResp
  { _mrAffectedRows     :: !Int
  , _mrReturningColumns :: ![ColumnValues a]
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''MutateResp)

-- | Postgres OIDs. <https://www.postgresql.org/docs/12/datatype-oid.html>
newtype OID = OID { unOID :: Int }
  deriving (Show, Eq, NFData, Hashable, ToJSON, FromJSON, Q.FromCol, Cacheable)

data Constraint
  = Constraint
  { _cName :: !PG.ConstraintName
  , _cOid  :: !OID
  } deriving (Show, Eq, Generic)
instance NFData Constraint
instance Hashable Constraint
instance Cacheable Constraint
$(deriveJSON (aesonDrop 2 snakeCase) ''Constraint)

data PrimaryKey a
  = PrimaryKey
  { _pkConstraint :: !Constraint
  , _pkColumns    :: !(NESeq a)
  } deriving (Show, Eq, Generic, Foldable)
instance (NFData a) => NFData (PrimaryKey a)
instance (Cacheable a) => Cacheable (PrimaryKey a)
$(makeLenses ''PrimaryKey)
$(deriveJSON (aesonDrop 3 snakeCase) ''PrimaryKey)

data ForeignKey (b :: BackendType)
  = ForeignKey
  { _fkConstraint    :: !Constraint
  , _fkForeignTable  :: !(TableName b)
  , _fkColumnMapping :: !(HM.HashMap (Column b) (Column b))
  } deriving (Generic)
deriving instance Backend b => Eq (ForeignKey b)
deriving instance Backend b => Show (ForeignKey b)
instance Backend b => NFData (ForeignKey b)
instance Backend b => Hashable (ForeignKey b)
instance Backend b => Cacheable (ForeignKey b)
instance Backend b => ToJSON (ForeignKey b) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase
instance Backend b => FromJSON (ForeignKey b) where
  parseJSON = genericParseJSON $ aesonDrop 3 snakeCase

data InpValInfo
  = InpValInfo
  { _iviDesc   :: !(Maybe G.Description)
  , _iviName   :: !G.Name
  , _iviDefVal :: !(Maybe (G.Value Void))
  , _iviType   :: !G.GType
  } deriving (Show, Eq, TH.Lift, Generic)
instance Cacheable InpValInfo

instance EquatableGType InpValInfo where
  type EqProps InpValInfo = (G.Name, G.GType)
  getEqProps ity = (,) (_iviName ity) (_iviType ity)

-- | Typeclass for equating relevant properties of various GraphQL types defined below
class EquatableGType a where
  type EqProps a
  getEqProps :: a -> EqProps a

type CustomColumnNames = HM.HashMap PG.PGCol G.Name

newtype SystemDefined = SystemDefined { unSystemDefined :: Bool }
  deriving (Show, Eq, FromJSON, ToJSON, Q.ToPrepArg, NFData, Cacheable)

isSystemDefined :: SystemDefined -> Bool
isSystemDefined = unSystemDefined

newtype SQLGenCtx
  = SQLGenCtx
  { stringifyNum :: Bool
  } deriving (Show, Eq)

successMsg :: EncJSON
successMsg = "{\"message\":\"success\"}"

newtype NonNegativeInt = NonNegativeInt { getNonNegativeInt :: Int }
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable, Num)

mkNonNegativeInt :: Int -> Maybe NonNegativeInt
mkNonNegativeInt x = case x >= 0 of
  True  -> Just $ NonNegativeInt x
  False -> Nothing

unsafeNonNegativeInt :: Int -> NonNegativeInt
unsafeNonNegativeInt = NonNegativeInt

instance FromJSON NonNegativeInt where
  parseJSON = withScientific "NonNegativeInt" $ \t -> do
    case t >= 0 of
      True  -> maybe (fail "integer passed is out of bounds") (pure . NonNegativeInt) $ toBoundedInteger t
      False -> fail "negative value not allowed"

newtype NonNegativeDiffTime = NonNegativeDiffTime { unNonNegativeDiffTime :: DiffTime }
  deriving (Show, Eq,ToJSON,Generic, NFData, Cacheable, Num)

unsafeNonNegativeDiffTime :: DiffTime -> NonNegativeDiffTime
unsafeNonNegativeDiffTime = NonNegativeDiffTime

mkNonNegativeDiffTime :: DiffTime -> Maybe NonNegativeDiffTime
mkNonNegativeDiffTime x = case x >= 0 of
  True  -> Just $ NonNegativeDiffTime x
  False -> Nothing

instance FromJSON NonNegativeDiffTime where
  parseJSON = withScientific "NonNegativeDiffTime" $ \t -> do
    case t >= 0 of
      True  -> return $ NonNegativeDiffTime . realToFrac $ t
      False -> fail "negative value not allowed"

newtype ResolvedWebhook
  = ResolvedWebhook { unResolvedWebhook :: Text}
  deriving ( Show, Eq, FromJSON, ToJSON, Hashable, ToTxt)

newtype InputWebhook
  = InputWebhook {unInputWebhook :: URLTemplate}
  deriving (Show, Eq, Generic)
instance NFData InputWebhook
instance Cacheable InputWebhook

instance ToJSON InputWebhook where
  toJSON =  String . printURLTemplate . unInputWebhook

instance FromJSON InputWebhook where
  parseJSON = withText "String" $ \t ->
    case parseURLTemplate t of
      Left e  -> fail $ "Parsing URL template failed: " ++ e
      Right v -> pure $ InputWebhook v

instance Q.FromCol InputWebhook where
  fromCol bs = do
    urlTemplate <- parseURLTemplate <$> Q.fromCol bs
    bimap (\e -> "Parsing URL template failed: " <> T.pack e) InputWebhook urlTemplate

resolveWebhook :: QErrM m => Env.Environment -> InputWebhook -> m ResolvedWebhook
resolveWebhook env (InputWebhook urlTemplate) = do
  let eitherRenderedTemplate = renderURLTemplate env urlTemplate
  either (throw400 Unexpected . T.pack)
    (pure . ResolvedWebhook) eitherRenderedTemplate

newtype Timeout = Timeout { unTimeout :: Int }
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable)

instance FromJSON Timeout where
  parseJSON = withScientific "Timeout" $ \t -> do
    timeout <- onNothing (toBoundedInteger t) $ fail (show t <> " is out of bounds")
    case timeout >= 0 of
      True  -> return $ Timeout timeout
      False -> fail "timeout value cannot be negative"

instance Arbitrary Timeout where
  arbitrary = Timeout <$> QC.choose (0, 10000000)

defaultActionTimeoutSecs :: Timeout
defaultActionTimeoutSecs = Timeout 30

data UrlConf
  = UrlValue !InputWebhook
  | UrlFromEnv !T.Text
  deriving (Show, Eq, Generic)
instance NFData UrlConf
instance Cacheable UrlConf

instance ToJSON UrlConf where
  toJSON (UrlValue w)      = toJSON w
  toJSON (UrlFromEnv wEnv) = object ["from_env" .= wEnv ]

instance FromJSON UrlConf where
  parseJSON (Object o) = UrlFromEnv <$> o .: "from_env"
  parseJSON t@(String _) =
    case (fromJSON t) of
      Error s   -> fail s
      Success a -> pure $ UrlValue a
  parseJSON _          = fail "one of string or object must be provided for url/webhook"

resolveUrlConf
  :: MonadError QErr m => Env.Environment -> UrlConf -> m Text
resolveUrlConf env = \case
  UrlValue v        -> unResolvedWebhook <$> resolveWebhook env v
  UrlFromEnv envVar -> getEnv env envVar

getEnv :: QErrM m => Env.Environment -> T.Text -> m T.Text
getEnv env k = do
  let mEnv = Env.lookupEnv env (T.unpack k)
  case mEnv of
    Nothing     -> throw400 NotFound $ "environment variable '" <> k <> "' not set"
    Just envVal -> return (T.pack envVal)
