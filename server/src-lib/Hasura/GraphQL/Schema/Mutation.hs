{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema.Mutation
  ( deleteFromTable
  , deleteFromTableByPk
  ) where


import           Hasura.Prelude

import           Data.Int                      (Int32)

import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Delete         as RQL
import qualified Hasura.RQL.DML.Returning      as RQL


import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser,
                                                UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column  (qualifiedObjectToName)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types



-- deletion

deleteFromTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> DelPermInfo          -- ^ delete permissions of the table
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (RQL.AnnDelG UnpreparedValue)))
deleteFromTable table fieldName description selectPerms deletePerms stringifyNum = do
  let whereName = $$(G.litName "where")
      whereDesc = G.Description "filter the rows which have to be deleted"
  whereArg  <- P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  selection <- mutationSelectionSet table selectPerms stringifyNum
  columns   <- tableSelectColumns table selectPerms
  pure $ P.selection fieldName description whereArg (RQL.MOutMultirowFields <$> selection)
    `mapField` mkDeleteObject table columns deletePerms


deleteFromTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> DelPermInfo          -- ^ delete permissions of the table
  -> Bool
  -> m (Maybe (FieldsParser 'Output n (Maybe (RQL.AnnDelG UnpreparedValue))))
deleteFromTableByPk table fieldName description selectPerms deletePerms stringifyNum = do
  tablePrimaryKeys <- _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  tableColumns     <- tableSelectColumns table selectPerms
  join <$> for tablePrimaryKeys \(_pkColumns -> columns) -> do
    if any (\c -> not $ pgiColumn c `Set.member` spiCols selectPerms) columns
    then pure Nothing
    else do
      argsParser <- fmap (BoolAnd . toList) . sequenceA <$> for columns \columnInfo -> do
        field <- P.column (pgiType columnInfo) (G.Nullability False)
        pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
          P.field (pgiName columnInfo) (pgiDescription columnInfo) field
      selectionSetParser <- tableFields table selectPerms stringifyNum
      pure $ Just $ P.selection fieldName description argsParser selectionSetParser
        `mapField` mkDel tableColumns
  -- WIP NOTE: I don't really like mapField and <&> to transform (Fields)Parsers...
  -- Some of them will go away if we harmonize string types, as a lot of it is just
  -- G.Name -> FieldName
  -- But a lot of those functions do end up applying the same kind of functions on
  -- the same kind of parsers, and I think there's an opportunity for some helper
  -- functions / operators to be introduced when the code is mostly finalized,
  -- before review.
  -- I'd like to push as much of the piping out of those functions as possible,
  -- to make them more readable...
  where
    mkDel columns (name, whereExpr, annotatedFields) = mkDeleteObject table columns deletePerms
      (name, whereExpr, RQL.MOutMultirowFields [(FieldName $ G.unName name, RQL.MRet annotatedFields)])


mkDeleteObject
  :: QualifiedTable
  -> [PGColumnInfo]
  -> DelPermInfo
  -> (G.Name, AnnBoolExp UnpreparedValue, RQL.MutationOutputG UnpreparedValue)
  -> RQL.AnnDelG UnpreparedValue
mkDeleteObject table columns delPerms (_, whereExp, mutationOutput) =
  RQL.AnnDel { RQL.dqp1Table   = table
             , RQL.dqp1Where   = (permissionFilter, whereExp)
             , RQL.dqp1Output  = mutationOutput
             -- TODO: is this correct?
             -- I'm only passing the columns that the user has SELECT access to
             -- while the code suggests that this should be *ALL* columns
             , RQL.dqp1AllCols = columns
             }
  where
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ dpiFilter delPerms



-- common

mutationSelectionSet
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> Bool
  -> m (Parser 'Output n (RQL.MutFldsG UnpreparedValue))
mutationSelectionSet table selectPermissions stringifyNum = do
  tableName <- qualifiedObjectToName table
  tableSet  <- tableFields table selectPermissions stringifyNum
  let affectedRowsName = $$(G.litName "affected_rows")
      affectedRowsDesc = G.Description "number of rows affected by the mutation"
      selectionName    = tableName <> $$(G.litName "_mutation_response")
      selectionDesc    = G.Description $ "response of any mutation on the table \"" <> G.unName tableName <> "\""
      returningName    = $$(G.litName "returning")
      returningDesc    = G.Description "data from the rows affected by the mutation"
      typenameRepr     = (FieldName "__typename", RQL.MExp $ G.unName selectionName)
      dummyIntParser   = undefined :: Parser 'Output n Int32
      selectionFields  = catMaybes <$> sequenceA
        [ P.selection_ affectedRowsName (Just affectedRowsDesc) dummyIntParser
          `mapField` (FieldName . G.unName *** const RQL.MCount)
        , P.selection_ returningName  (Just returningDesc) tableSet
          `mapField` (FieldName . G.unName *** RQL.MRet)
        ]
  pure $ P.selectionSet selectionName (Just selectionDesc) typenameRepr selectionFields
