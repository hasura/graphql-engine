module Hasura.RQL.Types.Common
       ( RelName(..)
       , relNameToTxt
       , RelType(..)
       , rootRelName
       , relTypeToTxt
       , RelInfo(..)

       , Backend (..)

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
       , EquatableGType(..)
       , InpValInfo(..)
       , CustomColumnNames

       , adminText
       , rootText

       , SystemDefined(..)
       , isSystemDefined

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
       ) where

import           Hasura.Prelude

import qualified Data.Environment                   as Env
import qualified Data.HashMap.Strict                as HM
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q
import qualified Language.GraphQL.Draft.Syntax      as G
import qualified Language.Haskell.TH.Syntax         as TH
import qualified PostgreSQL.Binary.Decoding         as PD
import qualified Test.QuickCheck                    as QC

import           Control.Lens                       (makeLenses)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Bifunctor                     (bimap)
import           Data.Kind                          (Type)
import           Data.Scientific                    (toBoundedInteger)
import           Data.Text.Extended
import           Data.Text.NonEmpty
import           Data.Typeable
import           Data.URL.Template
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Hasura.Backends.Postgres.SQL.DML   as PG
import qualified Hasura.Backends.Postgres.SQL.Types as PG

import           Hasura.EncJSON
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.DDL.Headers             ()
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend


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
  , Representable (XAILIKE b)
  , Representable (XANILIKE b)
  , Lift (TableName b)
  , Lift (BasicOrderType b)
  , Lift (NullsOrderType b)
  , Data (TableName b)
  , Data (ScalarType b)
  , Data (SQLExpression b)
  , FromJSON (BasicOrderType b)
  , FromJSON (NullsOrderType b)
  , ToJSON (BasicOrderType b)
  , ToJSON (NullsOrderType b)
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
  type ScalarType     b :: Type
  type SQLExpression  b :: Type
  type SQLOperator    b :: Type
  type XAILIKE        b :: Type
  type XANILIKE       b :: Type

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
  type ScalarType     'Postgres = PG.PGScalarType
  type SQLExpression  'Postgres = PG.SQLExp
  type SQLOperator    'Postgres = PG.SQLOp
  type XAILIKE        'Postgres = ()
  type XANILIKE       'Postgres = ()

-- instance Backend 'Mysql where
--   type XAILIKE 'MySQL = Void
--   type XANILIKE 'MySQL = Void


adminText :: NonEmptyText
adminText = mkNonEmptyTextUnsafe "admin"

rootText :: NonEmptyText
rootText = mkNonEmptyTextUnsafe "root"

newtype RelName
  = RelName { getRelTxt :: NonEmptyText }
  deriving (Show, Eq, Hashable, FromJSON, ToJSON, ToJSONKey, Q.ToPrepArg, Q.FromCol, Lift, Generic, Arbitrary, NFData, Cacheable)

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
    _        -> Nothing

data RelInfo
  = RelInfo
  { riName       :: !RelName
  , riType       :: !RelType
  , riMapping    :: !(HashMap PG.PGCol PG.PGCol)
  , riRTable     :: !PG.QualifiedTable
  , riIsManual   :: !Bool
  , riIsNullable :: !Bool
  } deriving (Show, Eq, Generic)
instance NFData RelInfo
instance Cacheable RelInfo
instance Hashable RelInfo
$(deriveToJSON (aesonDrop 2 snakeCase) ''RelInfo)

newtype FieldName
  = FieldName { getFieldNameTxt :: Text }
  deriving ( Show, Eq, Ord, Hashable, FromJSON, ToJSON
           , FromJSONKey, ToJSONKey, Lift, Data, Generic
           , IsString, Arbitrary, NFData, Cacheable
           , Semigroup
           )

instance PG.IsIdentifier FieldName where
  toIdentifier (FieldName f) = PG.Identifier f

instance ToTxt FieldName where
  toTxt (FieldName c) = c

fromPGCol :: PG.PGCol -> FieldName
fromPGCol c = FieldName $ PG.getPGColTxt c

fromRel :: RelName -> FieldName
fromRel = FieldName . relNameToTxt

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data WithTable a
  = WithTable
  { wtName :: !PG.QualifiedTable
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

type ColumnValues a = HM.HashMap PG.PGCol a

data MutateResp a
  = MutateResp
  { _mrAffectedRows     :: !Int
  , _mrReturningColumns :: ![ColumnValues a]
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''MutateResp)


type ColMapping = HM.HashMap PG.PGCol PG.PGCol

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

data ForeignKey
  = ForeignKey
  { _fkConstraint    :: !Constraint
  , _fkForeignTable  :: !PG.QualifiedTable
  , _fkColumnMapping :: !ColMapping
  } deriving (Show, Eq, Generic)
instance NFData ForeignKey
instance Hashable ForeignKey
instance Cacheable ForeignKey
$(deriveJSON (aesonDrop 3 snakeCase) ''ForeignKey)

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
  deriving ( Show, Eq, FromJSON, ToJSON, Hashable, ToTxt, Lift)

newtype InputWebhook
  = InputWebhook {unInputWebhook :: URLTemplate}
  deriving (Show, Eq, Lift, Generic)
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
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable, Lift)

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
  deriving (Show, Eq, Generic, Lift)
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
