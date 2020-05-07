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
import qualified Hasura.RQL.DML.Update         as RQL


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



-- update

updateTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> UpdPermInfo          -- ^ update permissions of the table
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (RQL.AnnUpdG UnpreparedValue)))
updateTable table fieldName description selectPerms updatePerms stringifyNum = do
  let whereName = $$(G.litName "where")
      whereDesc = G.Description "filter the rows which have to be updated"
  whereArg  <- P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  opArgs    <- updateOperators table updatePerms
  selection <- mutationSelectionSet table selectPerms stringifyNum
  columns   <- tableSelectColumns table selectPerms
  let argsParser = liftA2 (,) opArgs whereArg
  pure $ P.selection fieldName description argsParser (RQL.MOutMultirowFields <$> selection)
    `P.bindFields` traverse \(name, (opExps, whereExp), mutationOutput) ->
      -- FIXME: do all validation here
      -- this includes
      -- - parsing JSON elements
      -- - ensuring no column appears twice
      pure $ RQL.AnnUpd { RQL.uqp1Table   = table
                        , RQL.uqp1OpExps  = opExps
                        , RQL.uqp1Where   = (permissionFilter, whereExp)
                        , RQL.uqp1Check   = checkExp
                        , RQL.uqp1Output  = mutationOutput
                        -- TODO: is this correct?
                        -- I'm only passing the columns that the user has SELECT access to
                        -- while the code suggests that this should be *ALL* columns
                        , RQL.uqp1AllCols = columns
                        }
  where
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ upiFilter updatePerms
    checkExp = maybe annBoolExpTrue (fmapAnnBoolExp partialSQLExpToUnpreparedValue) $ upiCheck updatePerms


updateTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> UpdPermInfo          -- ^ update permissions of the table
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (RQL.AnnUpdG UnpreparedValue)))
updateTableByPk = undefined


updateOperators
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> UpdPermInfo          -- ^ update permissions of the table
  -> m (FieldsParser 'Input n [(PGColumnInfo, RQL.UpdOpExpG UnpreparedValue)])
updateOperators table updatePermissions = do
  tableName <- qualifiedObjectToName table
  columns   <- tableUpdateColumns table updatePermissions
  let intCols  = onlyIntCols   columns
      jsonCols = onlyJSONBCols columns
  fmap (concat . catMaybes) . sequenceA . catMaybes <$> sequenceA
    [ updateOperator tableName $$(G.litName "_set")
        columnParser RQL.UpdSet columns
        (G.Description "sets the columns of the filtered rows to the given values")
        (G.Description $ "input type for updating data in table \"" <> G.unName tableName <> "\"")
    , updateOperator tableName $$(G.litName "_inc")
        intParser RQL.UpdInc intCols
        (G.Description "increments the integer columns with given value of the filtered values")
        (G.Description $ "input type for incrementing integer columns in table \"" <> G.unName tableName <> "\"")

    -- WIP NOTE
    -- all json operators use the same description for the input type and the field
    -- i kept the same behaviour
    -- the comments also mention "_concat", but it seems it never existed
    -- i am guessing that's what prepend and append are for?
    , updateOperator tableName $$(G.litName "_prepend")
        textParser RQL.UpdPrepend jsonCols
        (G.Description "prepend existing jsonb value of filtered columns with new jsonb value")
        (G.Description "prepend existing jsonb value of filtered columns with new jsonb value")
    , updateOperator tableName $$(G.litName "_append")
        textParser RQL.UpdAppend jsonCols
        (G.Description "append existing jsonb value of filtered columns with new jsonb value")
        (G.Description "append existing jsonb value of filtered columns with new jsonb value")
    , updateOperator tableName $$(G.litName "_delete_key")
        textParser RQL.UpdDeleteKey jsonCols
        (G.Description "delete key/value pair or string element. key/value pairs are matched based on their key value")
        (G.Description "delete key/value pair or string element. key/value pairs are matched based on their key value")
    , updateOperator tableName $$(G.litName "_delete_elem")
        intParser RQL.UpdDeleteElem jsonCols
        (G.Description "delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array")
        (G.Description "delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array")
    , updateOperator tableName $$(G.litName "_delete_path_at")
        textParser RQL.UpdDeleteAtPath jsonCols
        (G.Description "delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array")
        (G.Description "delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array")
    ]
  where
    columnParser columnInfo = fmap P.mkParameter <$> P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    textParser   _          = fmap P.mkParameter <$> P.column (PGColumnScalar PGText)    (G.Nullability False)
    intParser    _          = fmap P.mkParameter <$> P.column (PGColumnScalar PGInteger) (G.Nullability False)

    updateOperator
      :: G.Name
      -> G.Name
      -> (PGColumnInfo -> m (Parser 'Both n UnpreparedValue))
      -> (UnpreparedValue -> RQL.UpdOpExpG UnpreparedValue)
      -> [PGColumnInfo]
      -> G.Description
      -> G.Description
      -> m (Maybe (FieldsParser 'Input n (Maybe [(PGColumnInfo, RQL.UpdOpExpG UnpreparedValue)])))
    updateOperator tableName opName mkParser updOpExp columns opDesc objDesc =
      if null columns then pure Nothing else do
        fields <- for columns \columnInfo -> do
          let fieldName = pgiName columnInfo
              fieldDesc = pgiDescription columnInfo
          fieldParser <- mkParser columnInfo
          pure $ P.fieldOptional fieldName fieldDesc fieldParser
            `mapField` \value -> (columnInfo, updOpExp value)
        let objName = tableName <> opName <> $$(G.litName "_input")
        pure $ Just $ P.fieldOptional opName (Just opDesc)
                    $ P.object objName (Just objDesc)
                    $ catMaybes <$> sequenceA fields



-- delete

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
