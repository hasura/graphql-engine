{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema.Mutation where
-- ( deleteFromTable
-- , deleteFromTableByPk
-- ) where


import           Hasura.Prelude

import           Data.Either                   (partitionEithers)
import           Data.Foldable                 (toList)
import           Data.Maybe                    (fromJust)
import           Data.Parser.JSONPath
import           Data.Traversable              (mapAccumL)

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Returning      as RQL
import qualified Hasura.RQL.DML.Select         as RQL
import qualified Hasura.SQL.DML                as SQL

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser,
                                                UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column  (qualifiedObjectToName)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value



-- deletion

-- deleteFromTable
--   :: forall m n. (MonadSchema n m, MonadError QErr m)
--   => QualifiedTable       -- ^ qualified name of the table
--   -> G.Name               -- ^ field display name
--   -> Maybe G.Description  -- ^ field description, if any
--   -> SelPermInfo          -- ^ select permissions of the table
--   -> DelPermInfo          -- ^ delete permissions of the table
--   -> Bool
--   -> m (FieldsParser 'Output n (Maybe (RQL.MutFldsG UnpreparedValue)))
-- deleteFromTable table fieldName description selectPerms deletePerms stringifyNum = do
--   let whereName = $$(G.litName "where")
--       whereDesc = G.Description "filter the rows which have to be deleted"
--   whereArg  <- P.field whereName (Just whereDesc) <$> boolExp table selectPerms
--   selection <- mutationSelectionSet table selectPerms stringifyNum
--   pure $ P.selection fieldName description whereArg selection
--     `mapField` undefined -- TODO
--
--
-- deleteFromTableByPk
--   :: forall m n. (MonadSchema n m, MonadError QErr m)
--   => QualifiedTable       -- ^ qualified name of the table
--   -> G.Name               -- ^ field display name
--   -> Maybe G.Description  -- ^ field description, if any
--   -> SelPermInfo          -- ^ select permissions of the table
--   -> DelPermInfo          -- ^ delete permissions of the table
--   -> Bool
--   -> m (Maybe (FieldsParser 'Output n (Maybe (RQL.MutFldsG UnpreparedValue))))
-- deleteFromTableByPk table fieldName description selectPerms deletePerms stringifyNum = do
--   tablePrimaryKeys <- _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
--   join <$> for tablePrimaryKeys \(_pkColumns -> columns) -> do
--     if any (\c -> not $ pgiColumn c `Set.member` spiCols selectPerms) columns
--     then pure Nothing
--     else do
--       argsParser <- sequenceA <$> for columns \columnInfo -> do
--         field <- P.column (pgiType columnInfo) (G.Nullability False)
--         pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
--           P.field (pgiName columnInfo) (pgiDescription columnInfo) field
--       selectionSetParser <- tableFields table selectPerms stringifyNum
--       pure $ Just $ P.selection fieldName description argsParser selectionSetParser
--         `mapField` undefined -- TODO



-- common

-- mutationSelectionSet
--   :: forall m n. (MonadSchema n m, MonadError QErr m)
--   => QualifiedTable
--   -> SelPermInfo
--   -> Bool
--   -> m (Parser 'Output n (Maybe (RQL.MutFldsG UnpreparedValue)))
-- mutationSelectionSet table selectPermissions stringifyNum = do
--   tableName <- qualifiedObjectToName table
--   tableSet  <- tableFields table selectPermissions stringifyNum
--   let affectedRowsName = $$(G.litName "affected_rows")
--       affectedRowsDesc = G.Description "number of rows affected by the mutation"
--       selectionName    = tableName <> $$(G.litName "_mutation_response")
--       selectionDesc    = G.Description $ "response of any mutation on the table \"" <> G.unName tableName <> "\""
--       returningName    = $$(G.litName "returning")
--       returningDesc    = G.Description "data from the rows affected by the mutation"
--       selectionFields  =
--         [ P.selection_ affectedRowsName (Just affectedRowsDesc) P.int
--           `mapField` const RQL.MCount
--         , P.selection_ returningName  (Just returningDesc) tableSet
--           `mapField` RQL.MRet
--         , typenameField $ const $ RQL.MExp $ G.unName selectionName
--         ]
--   pure $ P.selectionSet selectionName (Just selectionDesc) $ sequenceA selectionFields
