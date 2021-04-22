module Hasura.RQL.Types.Common
       ( RelName(..)
       , relNameToTxt
       , RelType(..)
       , relTypeToTxt

       , OID(..)

       , FieldName(..)

       , InsertOrder(..)

       , ToAesonPairs(..)

       , EquatableGType(..)
       , InpValInfo(..)

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

       , JsonAggSelect (..)

       , intScalar, floatScalar, stringScalar, boolScalar, idScalar
       , mkScalarTypeName

       , MetricsConfig(..)
       , emptyMetricsConfig
       ) where

import           Hasura.Prelude

import qualified Data.Environment                   as Env
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q
import qualified Language.GraphQL.Draft.Syntax      as G
import qualified Language.Haskell.TH.Syntax         as TH
import qualified PostgreSQL.Binary.Decoding         as PD
import qualified Test.QuickCheck                    as QC

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Bifunctor                     (bimap)
import           Data.Scientific                    (toBoundedInteger)
import           Data.Text.Extended
import           Data.Text.NonEmpty
import           Data.URL.Template


import qualified Hasura.Backends.Postgres.SQL.Types as PG

import           Hasura.EncJSON
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.DDL.Headers             ()
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend                 (BackendType)
import           Hasura.SQL.Types

newtype RelName
  = RelName { getRelTxt :: NonEmptyText }
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, ToJSONKey
           , Q.ToPrepArg, Q.FromCol, Generic, Arbitrary, NFData, Cacheable)

instance PG.IsIdentifier RelName where
  toIdentifier rn = PG.Identifier $ relNameToTxt rn

instance ToTxt RelName where
  toTxt = relNameToTxt

relNameToTxt :: RelName -> Text
relNameToTxt = unNonEmptyText . getRelTxt

relTypeToTxt :: RelType -> Text
relTypeToTxt ObjRel = "object"
relTypeToTxt ArrRel = "array"

data JsonAggSelect
  = JASMultipleRows
  | JASSingleObject
  deriving (Show, Eq, Generic)
instance Hashable JsonAggSelect

instance ToJSON JsonAggSelect where
  toJSON = \case
    JASMultipleRows -> "multiple_rows"
    JASSingleObject -> "single_row"

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

data InsertOrder = BeforeParent | AfterParent
   deriving (Show, Eq, Generic)

instance NFData InsertOrder
instance Hashable InsertOrder
instance Cacheable InsertOrder

instance FromJSON InsertOrder where
  parseJSON (String t)
    | t == "before_parent" = pure BeforeParent
    | t == "after_parent"  = pure AfterParent
  parseJSON _ =
    fail "insertion_order should be 'before_parent' or 'after_parent'"

instance ToJSON InsertOrder where
  toJSON = \case
    BeforeParent -> String "before_parent"
    AfterParent  -> String "after_parent"

-- should this be parameterized by both the source and the destination backend?
data RelInfo (b :: BackendType)
  = RelInfo
  { riName        :: !RelName
  , riType        :: !RelType
  , riMapping     :: !(HashMap (Column b) (Column b))
  , riRTable      :: !(TableName b)
  , riIsManual    :: !Bool
  , riIsNullable  :: !Bool
  , riInsertOrder :: !InsertOrder
  } deriving (Generic)
deriving instance Backend b => Show (RelInfo b)
deriving instance Backend b => Eq   (RelInfo b)
instance Backend b => NFData (RelInfo b)
instance Backend b => Cacheable (RelInfo b)
instance Backend b => Hashable (RelInfo b)
instance Backend b => FromJSON (RelInfo b) where
  parseJSON = genericParseJSON hasuraJSON
instance Backend b => ToJSON (RelInfo b) where
  toJSON = genericToJSON hasuraJSON

-- | Postgres OIDs. <https://www.postgresql.org/docs/12/datatype-oid.html>
newtype OID = OID { unOID :: Int }
  deriving (Show, Eq, NFData, Hashable, ToJSON, FromJSON, Q.FromCol, Cacheable)

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

newtype SystemDefined = SystemDefined { unSystemDefined :: Bool }
  deriving (Show, Eq, FromJSON, ToJSON, Q.ToPrepArg, NFData, Cacheable)

isSystemDefined :: SystemDefined -> Bool
isSystemDefined = unSystemDefined

data SQLGenCtx
  = SQLGenCtx
  { stringifyNum             :: Bool
  , dangerousBooleanCollapse :: Bool
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
  deriving (Show, Eq, Generic, Arbitrary)
instance NFData InputWebhook
instance Cacheable InputWebhook
instance Hashable InputWebhook

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
instance Hashable UrlConf

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

instance Arbitrary UrlConf where
  arbitrary = genericArbitrary

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

-- default scalar names
intScalar, floatScalar, stringScalar, boolScalar, idScalar :: G.Name
intScalar    = $$(G.litName "Int")
floatScalar  = $$(G.litName "Float")
stringScalar = $$(G.litName "String")
boolScalar   = $$(G.litName "Boolean")
idScalar     = $$(G.litName "ID")

-- TODO: This has to move into a Postgres specific module
mkScalarTypeName :: MonadError QErr m => PG.PGScalarType -> m G.Name
mkScalarTypeName PG.PGInteger  = pure intScalar
mkScalarTypeName PG.PGBoolean  = pure boolScalar
mkScalarTypeName PG.PGFloat    = pure floatScalar
mkScalarTypeName PG.PGText     = pure stringScalar
mkScalarTypeName PG.PGVarchar  = pure stringScalar
mkScalarTypeName scalarType = G.mkName (toSQLTxt scalarType) `onNothing` throw400 ValidationFailed
  ("cannot use SQL type " <> scalarType <<> " in the GraphQL schema because its name is not a "
  <> "valid GraphQL identifier")

-- | Various user-controlled configuration for metrics used by Pro
data MetricsConfig
  = MetricsConfig
  { _mcAnalyzeQueryVariables :: !Bool
  -- ^ should the query-variables be logged and analyzed for metrics
  , _mcAnalyzeResponseBody   :: !Bool
  -- ^ should the response-body be analyzed for empty and null responses
  } deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''MetricsConfig)

emptyMetricsConfig :: MetricsConfig
emptyMetricsConfig = MetricsConfig False False
