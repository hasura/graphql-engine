module Hasura.SQL.Builder where

import           Hasura.Prelude

import qualified Data.Text        as T
import qualified Text.Builder     as TB

import           Hasura.SQL.Text
import           Hasura.SQL.Types


data SQLBuilder = SQLBuilder
  { sqlbLiteral    :: Text -> TB.Builder
  , sqlbIdentifier :: Identifier -> TB.Builder
  }

class ToSQL a where
  toSQL :: SQLBuilder -> a -> TB.Builder

instance ToSQL a => ToSQL (Maybe a) where
  toSQL b = maybe mempty (toSQL b)


instance ToSQL Identifier where
  toSQL = sqlbIdentifier

instance ToSQL TableName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL FunctionName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL SchemaName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL ConstraintName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL PGScalarType where
  toSQL _ = TB.text . toTxt

instance ToSQL PGCol where
  toSQL b = toSQL b . toIdentifier

instance ToSQL a => ToSQL (QualifiedObject a) where
  toSQL b (QualifiedObject sn o) = toSQL b sn <> "." <> toSQL b o

instance ToSQL a => ToSQL (PGType a) where
  toSQL b = \case
    PGTypeScalar ty -> toSQL b ty
    -- typename array is an sql standard way of declaring types
    PGTypeArray ty -> toSQL b ty <> " array"


toSQLTxt :: ToSQL a => SQLBuilder -> a -> T.Text
toSQLTxt b a = TB.run $ toSQL b a

-- this uses a hardcoded equivalent of the postgres SQL builder
-- it is temporary and should not be used
unsafeToSQLTxt :: ToSQL a => a -> T.Text
unsafeToSQLTxt = toSQLTxt $ SQLBuilder pgLiteral pgIdentifier
  where
    pgLiteral    = TB.text . pgFmtLit
    pgIdentifier = TB.text . pgFmtIden . toTxt


infixr 6 <->
(<->) :: TB.Builder -> TB.Builder -> TB.Builder
(<->) l r = l <> TB.char ' ' <> r
{-# INLINE (<->) #-}

paren :: TB.Builder -> TB.Builder
paren t = TB.char '(' <> t <> TB.char ')'
{-# INLINE paren #-}

commaSeparated :: [TB.Builder] -> TB.Builder
commaSeparated = TB.intercalate ", "
