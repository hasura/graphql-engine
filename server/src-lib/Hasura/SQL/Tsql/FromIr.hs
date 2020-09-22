{-# LANGUAGE ApplicativeDo #-}

-- | Translate from the DML to the TSql dialect.

module Hasura.SQL.Tsql.FromIr where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import           Data.Validation
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.SQL.DML as Ir
import           Hasura.SQL.Tsql.Types as Tsql
import qualified Hasura.RQL.Types.Common as Ir
import qualified Hasura.SQL.Types as Sql
import           Prelude

data Error
  = FromTypeUnsupported (Ir.SelectFromG Ir.SQLExp)
  | FieldTypeUnsupported (Ir.AnnFieldG Ir.SQLExp)
  deriving (Show, Eq)

newtype FromIr a = FromIr { runFromIr :: Validation (NonEmpty Error) a}
  deriving (Functor, Applicative)

fromSelectFields :: Ir.AnnSelectG (Ir.AnnFieldsG Ir.SQLExp) Ir.SQLExp -> FromIr Tsql.Select
fromSelectFields Ir.AnnSelectG { _asnFields
                               , _asnFrom
                               , _asnPerm = Ir.TablePerm { _tpLimit = mPermLimit
                                                         , _tpFilter = permFilter
                                                         }
                               , _asnArgs
                               , _asnStrfyNum
                               } = do
  case _asnFrom of
    Ir.FromTable Sql.QualifiedObject { qSchema = Sql.SchemaName schemaName -- TODO: Consider many x.y.z.
                                     , qName = Sql.TableName qname
                                     } -> do
      fields <-
        traverse
          (\(Ir.FieldName name, field) ->
             case field of
               _ -> do
                 value <-
                   case field of
                     Ir.AFExpression text ->
                       pure (Tsql.ValueExpression (Odbc.TextValue text))
                     _ -> FromIr (Failure (pure (FieldTypeUnsupported field)))
                 pure
                   (ExpressionProjection
                      Aliased
                        { aliasedThing = value
                        , aliasedAlias = Just (Alias {aliasText = name})
                        }))
          _asnFields
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
          , selectFrom =
              FromQualifiedTable
                (unaliased
                   (Qualified
                      { qualifiedThing = TableName {tableNameText = qname}
                      , qualifiedSchemaName =
                          Just (SchemaName {schemaNameParts = [schemaName]})
                      }))
          }
    _ -> FromIr (Failure (pure (FromTypeUnsupported _asnFrom)))

fromExpression :: Proxy Ir.SQLExp -> FromIr (Proxy Tsql.Expression)
fromExpression _ = pure Proxy

--------------------------------------------------------------------------------
-- Comments

uncommented :: a -> Commented a
uncommented a = Commented {commentedComment = Nothing, commentedThing = a}

unaliased :: a -> Aliased a
unaliased a = Aliased {aliasedAlias = Nothing, aliasedThing = a}
