{-# LANGUAGE ApplicativeDo #-}

-- | Translate from the DML to the TSql dialect.

module Hasura.SQL.Tsql.FromIr
  ( fromSelectFields
  , Error(..)
  , runFromIr
  , FromIr
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Control.Monad.Validate
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.SQL.DML as Ir
import           Hasura.SQL.Tsql.Types as Tsql
import qualified Hasura.RQL.Types.Common as Ir
import qualified Hasura.SQL.Types as Sql
import           Prelude

--------------------------------------------------------------------------------
-- Types

data Error
  = FromTypeUnsupported (Ir.SelectFromG Ir.SQLExp)
  | FieldTypeUnsupported (Ir.AnnFieldG Ir.SQLExp)
  deriving (Show, Eq)

newtype FromIr a = FromIr { runFromIr :: Validate (NonEmpty Error) a}
  deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------
-- Conversion functions

fromSelectFields :: Ir.AnnSelectG (Ir.AnnFieldsG Ir.SQLExp) Ir.SQLExp -> FromIr Tsql.Select
fromSelectFields annSelectG = do
  fields <- traverse fromProjection _asnFields
  selectFrom <-
    case _asnFrom of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      _ -> FromIr (refute (pure (FromTypeUnsupported _asnFrom)))
  pure
    Select
      { selectTop =
          case mPermLimit of
            Nothing -> uncommented NoTop
            Just limit ->
              Commented
                { commentedComment = pure DueToPermission
                , commentedThing = Top limit
                }
      , selectProjections = NE.fromList fields
      , selectFrom
      , selectWhere = NoWhere
      }
  where
    Ir.AnnSelectG {_asnFields, _asnFrom, _asnPerm, _asnArgs, _asnStrfyNum} =
      annSelectG
    Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = _asnPerm

fromQualifiedTable :: Sql.QualifiedObject Sql.TableName -> FromIr From
fromQualifiedTable qualifiedObject =
  pure
    (FromQualifiedTable
       (unaliased
          (Qualified
             { qualifiedThing = TableName {tableNameText = qname}
             , qualifiedSchemaName =
                 Just (SchemaName {schemaNameParts = [schemaName]})
             })))
  where
    Sql.QualifiedObject { qSchema = Sql.SchemaName schemaName
                         -- TODO: Consider many x.y.z. in schema name.
                        , qName = Sql.TableName qname
                        } = qualifiedObject

fromProjection :: (Ir.FieldName, Ir.AnnFieldG Ir.SQLExp) -> FromIr Projection
fromProjection (Ir.FieldName name, field) =
  case field of
    _ -> do
      value <-
        case field of
          Ir.AFExpression text ->
            pure (Tsql.ValueExpression (Odbc.TextValue text))
          _ -> FromIr (refute (pure (FieldTypeUnsupported field)))
      pure
        (ExpressionProjection
           Aliased
             { aliasedThing = value
             , aliasedAlias = Just (Alias {aliasText = name})
             })

--------------------------------------------------------------------------------
-- Comments

uncommented :: a -> Commented a
uncommented a = Commented {commentedComment = Nothing, commentedThing = a}

unaliased :: a -> Aliased a
unaliased a = Aliased {aliasedAlias = Nothing, aliasedThing = a}
