module Hasura.SQL.Types
  ( Identifier(..)
  , IsIdentifier(..)

  , TableName(..)
  , ConstraintName(..)
  , FunctionName(..)
  , PGDescription(..)

  , SchemaName(..)
  , publicSchema
  , hdbCatalogSchema

  , QualifiedObject(..)
  , QualifiedTable
  , QualifiedFunction
  , qualifiedObjectToText
  , snakeCaseQualifiedObject
  , qualifiedObjectToName

  , PGCol
  , unsafePGCol
  , getPGColTxt
  , showPGCols

  , PGScalarType(..)
  , PGType(..)
  , PGTypeKind(..)
  , WithScalarType(..)
  , geoTypes
  , isBigNum
  , isComparableType
  , isGeoType
  , isIntegerType
  , isJSONType
  , isNumType
  , isStringType
  , pgTypeOid
  , stringTypes
  , textToPGScalarType

  , QualifiedPGType(..)
  , isBaseType
  , typeToTable
  )
where


import           Hasura.Prelude

import qualified Data.Text.Extended            as T
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified Language.GraphQL.Draft.Syntax as G
-- import qualified PostgreSQL.Binary.Decoding    as PD

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Encoding           (text)
import           Data.Aeson.TH
import           Data.Aeson.Types              (toJSONKeyText)
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Database.PG.Query             as Q
import qualified Database.PG.Query.PTI         as PTI

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Text



-- identifier

newtype Identifier
  = Identifier { getIdenTxt :: T.Text }
  deriving (Show, Eq, NFData, FromJSON, ToJSON, Hashable, Semigroup, Data, Cacheable)

instance ToTxt Identifier where
  toTxt = getIdenTxt
  {-# INLINE toTxt #-}

class IsIdentifier a where
  toIdentifier :: a -> Identifier

instance IsIdentifier Identifier where
  toIdentifier = id
  {-# INLINE toIdentifier #-}



-- table name

newtype TableName
  = TableName { getTableTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift, Data
           , Generic, Arbitrary, NFData, Cacheable, IsString )

instance ToTxt TableName where
  toTxt = getTableTxt
  {-# INLINE toTxt #-}

instance IsIdentifier TableName where
  toIdentifier = Identifier . getTableTxt
  {-# INLINE toIdentifier #-}



-- table type

{-

data TableType
  = TTBaseTable
  | TTView
  | TTForeignTable
  | TTLocalTemporary
  deriving (Eq)

instance Show TableType where
  show = T.unpack . tableTypeToTxt

instance ToTxt TableType where
  toTxt = tableTypeToTxt
  {-# INLINE toTxt #-}

instance Q.FromCol TableType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "BASE TABLE"      -> Just TTBaseTable
    "VIEW"            -> Just TTView
    "FOREIGN TABLE"   -> Just TTForeignTable
    "LOCAL TEMPORARY" -> Just TTLocalTemporary
    _                 -> Nothing

tableTypeToTxt :: TableType -> T.Text
tableTypeToTxt TTBaseTable      = "BASE TABLE"
tableTypeToTxt TTView           = "VIEW"
tableTypeToTxt TTForeignTable   = "FOREIGN TABLE"
tableTypeToTxt TTLocalTemporary = "LOCAL TEMPORARY"

isView :: TableType -> Bool
isView TTView = True
isView _      = False

-}



-- constraint name

newtype ConstraintName
  = ConstraintName { getConstraintTxt :: T.Text }
  deriving (Show, Eq, ToTxt, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift, NFData, Cacheable)

instance IsIdentifier ConstraintName where
  toIdentifier = Identifier . getConstraintTxt
  {-# INLINE toIdentifier #-}



-- function name

newtype FunctionName
  = FunctionName { getFunctionTxt :: T.Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift, Data, Generic, Arbitrary, NFData, Cacheable)

instance IsIdentifier FunctionName where
  toIdentifier = Identifier . getFunctionTxt
  {-# INLINE toIdentifier #-}

instance ToTxt FunctionName where
  toTxt = getFunctionTxt
  {-# INLINE toTxt #-}



-- schema name

newtype SchemaName
  = SchemaName { getSchemaTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift, Data, Generic
           , Arbitrary, NFData, Cacheable, IsString )

instance IsIdentifier SchemaName where
  toIdentifier = Identifier . getSchemaTxt
  {-# INLINE toIdentifier #-}

instance ToTxt SchemaName where
  toTxt = getSchemaTxt
  {-# INLINE toTxt #-}


publicSchema :: SchemaName
publicSchema = SchemaName "public"

hdbCatalogSchema :: SchemaName
hdbCatalogSchema = SchemaName "hdb_catalog"



-- qualified object

type QualifiedTable    = QualifiedObject TableName
type QualifiedFunction = QualifiedObject FunctionName

data QualifiedObject a
  = QualifiedObject
  { qSchema :: !SchemaName
  , qName   :: !a
  } deriving (Show, Eq, Functor, Ord, Generic, Lift, Data)
instance (NFData a) => NFData (QualifiedObject a)
instance (Cacheable a) => Cacheable (QualifiedObject a)

instance (FromJSON a) => FromJSON (QualifiedObject a) where
  parseJSON v@(String _) =
    QualifiedObject publicSchema <$> parseJSON v
  parseJSON (Object o) =
    QualifiedObject <$>
    o .:? "schema" .!= publicSchema <*>
    o .: "name"
  parseJSON _ =
    fail "expecting a string/object for QualifiedObject"

instance (ToJSON a) => ToJSON (QualifiedObject a) where
  toJSON (QualifiedObject sn o) =
    object [ "schema" .= sn
           , "name"  .= o
           ]

instance (ToJSON a, ToTxt a) => ToJSONKey (QualifiedObject a) where
  toJSONKey = ToJSONKeyText qualifiedObjectToText (text . qualifiedObjectToText)

instance (ToTxt a) => ToTxt (QualifiedObject a) where
  toTxt = qualifiedObjectToText
  {-# INLINE toTxt #-}

instance (Hashable a) => Hashable (QualifiedObject a)


qualifiedObjectToText :: ToTxt a => QualifiedObject a -> T.Text
qualifiedObjectToText (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "." <> toTxt o

snakeCaseQualifiedObject :: ToTxt a => QualifiedObject a -> T.Text
snakeCaseQualifiedObject (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "_" <> toTxt o

qualifiedObjectToName :: (ToTxt a, MonadError QErr m) => QualifiedObject a -> m G.Name
qualifiedObjectToName objectName = do
  let textName = snakeCaseQualifiedObject objectName
  onNothing (G.mkName textName) $ throw400 ValidationFailed $
    "cannot include " <> objectName <<> " in the GraphQL schema because " <> textName
    <<> " is not a valid GraphQL identifier"



-- description

newtype PGDescription
  = PGDescription { getPGDescription :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Q.FromCol, NFData, Cacheable, Hashable)



-- postgres column name

newtype PGCol
  = PGCol { getPGColTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, ToJSONKey
           , FromJSONKey, Lift, Data, Generic, Arbitrary, NFData, Cacheable, IsString )

instance IsIdentifier PGCol where
  toIdentifier = Identifier . getPGColTxt
  {-# INLINE toIdentifier #-}

instance ToTxt PGCol where
  toTxt = getPGColTxt
  {-# INLINE toTxt #-}


unsafePGCol :: Text -> PGCol
unsafePGCol = PGCol

showPGCols :: (Foldable t) => t PGCol -> T.Text
showPGCols cols =
  T.intercalate ", " $ map (T.dquote . getPGColTxt) $ toList cols



-- postgres scalar type

data PGScalarType
  = PGSmallInt
  | PGInteger
  | PGBigInt
  | PGSerial
  | PGBigSerial
  | PGFloat
  | PGDouble
  | PGNumeric
  | PGMoney
  | PGBoolean
  | PGChar
  | PGVarchar
  | PGText
  | PGCitext
  | PGDate
  | PGTimeStamp
  | PGTimeStampTZ
  | PGTimeTZ
  | PGJSON
  | PGJSONB
  | PGGeometry
  | PGGeography
  | PGRaster
  | PGUUID
  | PGUnknown !T.Text
  deriving (Show, Eq, Ord, Lift, Generic, Data)
instance NFData PGScalarType
instance Hashable PGScalarType
instance Cacheable PGScalarType

instance ToTxt PGScalarType where
  toTxt = \case
    PGSmallInt    -> "smallint"
    PGInteger     -> "integer"
    PGBigInt      -> "bigint"
    PGSerial      -> "serial"
    PGBigSerial   -> "bigserial"
    PGFloat       -> "real"
    PGDouble      -> "float8"
    PGNumeric     -> "numeric"
    PGMoney       -> "money"
    PGBoolean     -> "boolean"
    PGChar        -> "character"
    PGVarchar     -> "varchar"
    PGText        -> "text"
    PGCitext      -> "citext"
    PGDate        -> "date"
    PGTimeStamp   -> "timestamp"
    PGTimeStampTZ -> "timestamptz"
    PGTimeTZ      -> "timetz"
    PGJSON        -> "json"
    PGJSONB       -> "jsonb"
    PGGeometry    -> "geometry"
    PGGeography   -> "geography"
    PGRaster      -> "raster"
    PGUUID        -> "uuid"
    PGUnknown t   -> t

instance ToJSON PGScalarType where
  toJSON = String . toTxt

instance ToJSONKey PGScalarType where
  toJSONKey = toJSONKeyText toTxt


textToPGScalarType :: Text -> PGScalarType
textToPGScalarType t = fromMaybe (PGUnknown t) (lookup t pgScalarTranslations)

-- Inlining this results in pretty terrible Core being generated by GHC.
{-# NOINLINE pgScalarTranslations #-}
pgScalarTranslations :: [(Text, PGScalarType)]
pgScalarTranslations =
  [ ("serial"                      , PGSerial)
  , ("bigserial"                   , PGBigSerial)

  , ("smallint"                    , PGSmallInt)
  , ("int2"                        , PGSmallInt)

  , ("integer"                     , PGInteger)
  , ("int4"                        , PGInteger)

  , ("bigint"                      , PGBigInt)
  , ("int8"                        , PGBigInt)

  , ("real"                        , PGFloat)
  , ("float4"                      , PGFloat)

  , ("double precision"            , PGDouble)
  , ("float8"                      , PGDouble)

  , ("numeric"                     , PGNumeric)
  , ("decimal"                     , PGNumeric)

  , ("money"                       , PGMoney)

  , ("boolean"                     , PGBoolean)
  , ("bool"                        , PGBoolean)

  , ("character"                   , PGChar)

  , ("varchar"                     , PGVarchar)
  , ("character varying"           , PGVarchar)

  , ("text"                        , PGText)
  , ("citext"                      , PGCitext)

  , ("date"                        , PGDate)

  , ("timestamp"                   , PGTimeStamp)
  , ("timestamp without time zone" , PGTimeStamp)

  , ("timestamptz"                 , PGTimeStampTZ)
  , ("timestamp with time zone"    , PGTimeStampTZ)

  , ("timetz"                      , PGTimeTZ)
  , ("time with time zone"         , PGTimeTZ)

  , ("json"                        , PGJSON)
  , ("jsonb"                       , PGJSONB)

  , ("geometry"                    , PGGeometry)
  , ("geography"                   , PGGeography)

  , ("raster"                      , PGRaster)
  , ("uuid"                        , PGUUID)
  ]

instance FromJSON PGScalarType where
  parseJSON (String t) = return $ textToPGScalarType t
  parseJSON _          = fail "Expecting a string for PGScalarType"

pgTypeOid :: PGScalarType -> PQ.Oid
pgTypeOid PGSmallInt    = PTI.int2
pgTypeOid PGInteger     = PTI.int4
pgTypeOid PGBigInt      = PTI.int8
pgTypeOid PGSerial      = PTI.int4
pgTypeOid PGBigSerial   = PTI.int8
pgTypeOid PGFloat       = PTI.float4
pgTypeOid PGDouble      = PTI.float8
pgTypeOid PGNumeric     = PTI.numeric
pgTypeOid PGMoney       = PTI.numeric
pgTypeOid PGBoolean     = PTI.bool
pgTypeOid PGChar        = PTI.char
pgTypeOid PGVarchar     = PTI.varchar
pgTypeOid PGText        = PTI.text
pgTypeOid PGCitext      = PTI.text -- Explict type cast to citext needed, See also Note [Type casting prepared params]
pgTypeOid PGDate        = PTI.date
pgTypeOid PGTimeStamp   = PTI.timestamp
pgTypeOid PGTimeStampTZ = PTI.timestamptz
pgTypeOid PGTimeTZ      = PTI.timetz
pgTypeOid PGJSON        = PTI.json
pgTypeOid PGJSONB       = PTI.jsonb
pgTypeOid PGGeometry    = PTI.text -- we are using the ST_GeomFromGeoJSON($i) instead of $i
pgTypeOid PGGeography   = PTI.text
pgTypeOid PGRaster      = PTI.text -- we are using the ST_RastFromHexWKB($i) instead of $i
pgTypeOid PGUUID        = PTI.uuid
pgTypeOid (PGUnknown _) = PTI.auto

isIntegerType :: PGScalarType -> Bool
isIntegerType PGInteger  = True
isIntegerType PGSmallInt = True
isIntegerType PGBigInt   = True
isIntegerType _          = False

isNumType :: PGScalarType -> Bool
isNumType PGFloat   = True
isNumType PGDouble  = True
isNumType PGNumeric = True
isNumType PGMoney   = True
isNumType ty        = isIntegerType ty

stringTypes :: [PGScalarType]
stringTypes = [PGVarchar, PGText, PGCitext]

isStringType :: PGScalarType -> Bool
isStringType = (`elem` stringTypes)

jsonTypes :: [PGScalarType]
jsonTypes = [PGJSON, PGJSONB]

isJSONType :: PGScalarType -> Bool
isJSONType = (`elem` jsonTypes)

isComparableType :: PGScalarType -> Bool
isComparableType PGJSON        = False
isComparableType PGJSONB       = False
isComparableType PGGeometry    = False
isComparableType PGGeography   = False
isComparableType PGBoolean     = False
isComparableType (PGUnknown _) = False
isComparableType _             = True

isBigNum :: PGScalarType -> Bool
isBigNum = \case
  PGBigInt    -> True
  PGBigSerial -> True
  PGNumeric   -> True
  PGDouble    -> True
  PGMoney     -> True
  _           -> False

geoTypes :: [PGScalarType]
geoTypes = [PGGeometry, PGGeography]

isGeoType :: PGScalarType -> Bool
isGeoType = (`elem` geoTypes)

data WithScalarType a
  = WithScalarType
  { pstType  :: !PGScalarType
  , pstValue :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable)



-- postgres type

-- | The type of all Postgres types (i.e. scalars and arrays). This type is parameterized so that
-- we can have both @'PGType' 'PGScalarType'@ and @'PGType' 'Hasura.RQL.Types.PGColumnType'@, for
-- when we care about the distinction made by 'Hasura.RQL.Types.PGColumnType'. If we ever change
-- 'Hasura.RQL.Types.PGColumnType' to handle arrays, not just scalars, then the parameterization can
-- go away.
--
-- TODO (from master): This is incorrect modeling, as 'PGScalarType' will capture anything (under 'PGUnknown').
-- This should be fixed when support for all types is merged.
data PGType a
  = PGTypeScalar !a
  | PGTypeArray !a
  deriving (Show, Eq, Generic, Data, Functor)
instance (NFData a) => NFData (PGType a)
instance (Cacheable a) => Cacheable (PGType a)
$(deriveJSON defaultOptions{constructorTagModifier = drop 6} ''PGType)



-- postgres type kind

data PGTypeKind
  = PGKindBase
  | PGKindComposite
  | PGKindDomain
  | PGKindEnum
  | PGKindRange
  | PGKindPseudo
  | PGKindUnknown !T.Text
  deriving (Show, Eq, Generic)
instance NFData PGTypeKind
instance Cacheable PGTypeKind

instance FromJSON PGTypeKind where
  parseJSON = withText "postgresTypeKind" $
    \t -> pure $ case t of
      "b" -> PGKindBase
      "c" -> PGKindComposite
      "d" -> PGKindDomain
      "e" -> PGKindEnum
      "r" -> PGKindRange
      "p" -> PGKindPseudo
      _   -> PGKindUnknown t

instance ToJSON PGTypeKind where
  toJSON = \case
    PGKindBase      -> "b"
    PGKindComposite -> "c"
    PGKindDomain    -> "d"
    PGKindEnum      -> "e"
    PGKindRange     -> "r"
    PGKindPseudo    -> "p"
    PGKindUnknown t -> String t



-- qualified postgres type

data QualifiedPGType
  = QualifiedPGType
  { _qptSchema :: !SchemaName
  , _qptName   :: !PGScalarType
  , _qptType   :: !PGTypeKind
  } deriving (Show, Eq, Generic)
instance NFData QualifiedPGType
instance Cacheable QualifiedPGType
$(deriveJSON (aesonDrop 4 snakeCase) ''QualifiedPGType)

isBaseType :: QualifiedPGType -> Bool
isBaseType (QualifiedPGType _ n ty) =
  notUnknown && (ty == PGKindBase)
  where
    notUnknown = case n of
      PGUnknown _ -> False
      _           -> True

typeToTable :: QualifiedPGType -> QualifiedTable
typeToTable (QualifiedPGType sch n _) =
  QualifiedObject sch $ TableName $ toTxt n






-- instance ToSQL Iden where
--   toSQL (Iden t) =
--     TB.text $ pgFmtIden t

--
-- pgFmtLit :: T.Text -> T.Text
-- pgFmtLit x =
--  let trimmed = trimNullChars x
--      escaped = "'" <> T.replace "'" "''" trimmed <> "'"
--      slashed = T.replace "\\" "\\\\" escaped in
--  if "\\" `T.isInfixOf` escaped
--    then "E" <> slashed
--    else slashed
--
-- trimNullChars :: T.Text -> T.Text
-- trimNullChars = T.takeWhile (/= '\x0')

-- instance (ToSQL a) => ToSQL (Maybe a) where
--   toSQL (Just a) = toSQL a
--   toSQL Nothing  = mempty

-- instance (ToSQL a) => ToSQL (QualifiedObject a) where
--   toSQL (QualifiedObject sn o) =
--     toSQL sn <> "." <> toSQL o

-- instance (ToSQL a) => ToSQL (PGType a) where
--   toSQL = \case
--     PGTypeScalar ty -> toSQL ty
--     -- typename array is an sql standard way of declaring types
--     PGTypeArray ty -> toSQL ty <> " array"
