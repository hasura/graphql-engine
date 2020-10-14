module Hasura.SQL.Types
  ( ToSQL(..)
  , toSQLTxt

  , (<+>)
  , (<<>)
  , (<>>)

  , pgFmtLit
  , pgFmtIden
  , isView

  , QualifiedTable
  , QualifiedFunction

  , PGDescription(..)

  , PGCol
  , unsafePGCol
  , getPGColTxt
  , showPGCols

  , DQuote(..)
  , dquote
  , squote
  , dquoteList

  , IsIden(..)
  , Iden(..)

  , ToTxt(..)

  , SchemaName(..)
  , publicSchema
  , hdbCatalogSchema

  , TableName(..)
  , FunctionName(..)
  , ConstraintName(..)

  , QualifiedObject(..)
  , qualObjectToText
  , snakeCaseQualObject
  , qualifiedObjectToName
  )
where

import           Hasura.Prelude

import qualified Data.Text.Extended            as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G
import qualified PostgreSQL.Binary.Decoding    as PD
import qualified Text.Builder                  as TB

import           Data.Aeson
import           Data.Aeson.Encoding           (text)
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift)

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.Types.Error


class ToSQL a where
  toSQL :: a -> TB.Builder

instance ToSQL TB.Builder where
  toSQL x = x

toSQLTxt :: (ToSQL a) => a -> T.Text
toSQLTxt = TB.run . toSQL

infixr 6 <+>
(<+>) :: (ToSQL a) => T.Text -> [a] -> TB.Builder
(<+>) _ [] = mempty
(<+>) kat (x:xs) =
  toSQL x <> mconcat [ TB.text kat <> toSQL x' | x' <- xs ]
{-# INLINE (<+>) #-}

newtype Iden
  = Iden { getIdenTxt :: T.Text }
  deriving (Show, Eq, NFData, FromJSON, ToJSON, Hashable, Semigroup, Data, Cacheable)

instance ToSQL Iden where
  toSQL (Iden t) =
    TB.text $ pgFmtIden t

class IsIden a where
  toIden :: a -> Iden

instance IsIden Iden where
  toIden = id

class DQuote a where
  dquoteTxt :: a -> T.Text

instance DQuote T.Text where
  dquoteTxt = id
  {-# INLINE dquoteTxt #-}

instance DQuote G.Name where
  dquoteTxt = dquoteTxt . G.unName

deriving instance DQuote G.EnumValue

dquote :: (DQuote a) => a -> T.Text
dquote = T.dquote . dquoteTxt
{-# INLINE dquote #-}

squote :: (DQuote a) => a -> T.Text
squote = T.squote . dquoteTxt
{-# INLINE squote #-}

dquoteList :: (DQuote a, Foldable t) => t a -> T.Text
dquoteList = T.intercalate ", " . map dquote . toList
{-# INLINE dquoteList #-}

infixr 6 <>>
(<>>) :: (DQuote a) => T.Text -> a -> T.Text
(<>>) lTxt a = lTxt <> dquote a
{-# INLINE (<>>) #-}

infixr 6 <<>
(<<>) :: (DQuote a) => a -> T.Text -> T.Text
(<<>) a rTxt = dquote a <> rTxt
{-# INLINE (<<>) #-}

pgFmtIden :: T.Text -> T.Text
pgFmtIden x =
  "\"" <> T.replace "\"" "\"\"" (trimNullChars x) <> "\""

pgFmtLit :: T.Text -> T.Text
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> T.replace "'" "''" trimmed <> "'"
     slashed = T.replace "\\" "\\\\" escaped in
 if "\\" `T.isInfixOf` escaped
   then "E" <> slashed
   else slashed

trimNullChars :: T.Text -> T.Text
trimNullChars = T.takeWhile (/= '\x0')

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing  = mempty

class ToTxt a where
  toTxt :: a -> T.Text

newtype TableName
  = TableName { getTableTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift, Data
           , Generic, Arbitrary, NFData, Cacheable, IsString )

instance IsIden TableName where
  toIden (TableName t) = Iden t

instance DQuote TableName where
  dquoteTxt (TableName t) = t

instance ToSQL TableName where
  toSQL = toSQL . toIden

instance ToTxt TableName where
  toTxt = getTableTxt

data TableType
  = TTBaseTable
  | TTView
  | TTForeignTable
  | TTLocalTemporary
  deriving (Eq)

tableTyToTxt :: TableType -> T.Text
tableTyToTxt TTBaseTable      = "BASE TABLE"
tableTyToTxt TTView           = "VIEW"
tableTyToTxt TTForeignTable   = "FOREIGN TABLE"
tableTyToTxt TTLocalTemporary = "LOCAL TEMPORARY"

instance Show TableType where
  show = T.unpack . tableTyToTxt

instance Q.FromCol TableType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "BASE TABLE"      -> Just TTBaseTable
    "VIEW"            -> Just TTView
    "FOREIGN TABLE"   -> Just TTForeignTable
    "LOCAL TEMPORARY" -> Just TTLocalTemporary
    _                 -> Nothing

isView :: TableType -> Bool
isView TTView = True
isView _      = False

newtype ConstraintName
  = ConstraintName { getConstraintTxt :: T.Text }
  deriving (Show, Eq, DQuote, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift, NFData, Cacheable)

instance IsIden ConstraintName where
  toIden (ConstraintName t) = Iden t

instance ToSQL ConstraintName where
  toSQL = toSQL . toIden

newtype FunctionName
  = FunctionName { getFunctionTxt :: T.Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift, Data, Generic, Arbitrary, NFData, Cacheable)

instance IsIden FunctionName where
  toIden (FunctionName t) = Iden t

instance DQuote FunctionName where
  dquoteTxt (FunctionName t) = t

instance ToSQL FunctionName where
  toSQL = toSQL . toIden

instance ToTxt FunctionName where
  toTxt = getFunctionTxt

newtype SchemaName
  = SchemaName { getSchemaTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift, Data, Generic
           , Arbitrary, NFData, Cacheable, IsString )

publicSchema :: SchemaName
publicSchema = SchemaName "public"

hdbCatalogSchema :: SchemaName
hdbCatalogSchema = SchemaName "hdb_catalog"

instance IsIden SchemaName where
  toIden (SchemaName t) = Iden t

instance ToSQL SchemaName where
  toSQL = toSQL . toIden

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
  toJSONKey = ToJSONKeyText qualObjectToText (text . qualObjectToText)

instance (ToTxt a) => DQuote (QualifiedObject a) where
  dquoteTxt = qualObjectToText

instance (Hashable a) => Hashable (QualifiedObject a)

instance (ToSQL a) => ToSQL (QualifiedObject a) where
  toSQL (QualifiedObject sn o) =
    toSQL sn <> "." <> toSQL o

qualObjectToText :: ToTxt a => QualifiedObject a -> T.Text
qualObjectToText (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "." <> toTxt o

snakeCaseQualObject :: ToTxt a => QualifiedObject a -> T.Text
snakeCaseQualObject (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "_" <> toTxt o

qualifiedObjectToName :: (ToTxt a, MonadError QErr m) => QualifiedObject a -> m G.Name
qualifiedObjectToName objectName = do
  let textName = snakeCaseQualObject objectName
  onNothing (G.mkName textName) $ throw400 ValidationFailed $
    "cannot include " <> objectName <<> " in the GraphQL schema because " <> textName
    <<> " is not a valid GraphQL identifier"

type QualifiedTable = QualifiedObject TableName

type QualifiedFunction = QualifiedObject FunctionName

newtype PGDescription
  = PGDescription { getPGDescription :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Q.FromCol, NFData, Cacheable, Hashable)

newtype PGCol
  = PGCol { getPGColTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, ToJSONKey
           , FromJSONKey, Lift, Data, Generic, Arbitrary, NFData, Cacheable, IsString )

instance IsIden PGCol where
  toIden (PGCol t) = Iden t

instance ToSQL PGCol where
  toSQL = toSQL . toIden

instance DQuote PGCol where
  dquoteTxt (PGCol t) = t

unsafePGCol :: Text -> PGCol
unsafePGCol = PGCol

showPGCols :: (Foldable t) => t PGCol -> T.Text
showPGCols cols =
  T.intercalate ", " $ map (T.dquote . getPGColTxt) $ toList cols
