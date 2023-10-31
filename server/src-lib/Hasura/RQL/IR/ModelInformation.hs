module Hasura.RQL.IR.ModelInformation
  ( ModelType (..),
    ModelSourceType (..),
    ModelOperationType (..),
    ModelInfoPart (..),
    getModelInfoPartfromModelNames,
    -- Postgres
    getMutationInsertArgumentModelNamesPostgres,
    -- MSSQL
    getMutationInsertArgumentModelNamesMSSQL,
    -- DataConnector
    getMutationInsertArgumentModelNamesDC,
    -- Common
    irToModelInfoGen,
    getArgumentModelNamesGen,
    getRSModelInfoGen,
    getMutationOutputModelNamesGen,
  )
where

import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended
import Hasura.Backends.MSSQL.Types qualified as MSSQL
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Types.Insert
import Hasura.LogicalModel.IR
import Hasura.LogicalModel.Types
import Hasura.NativeQuery.IR
import Hasura.NativeQuery.Types (NativeQueryName (..))
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.ModelInformation.Types
import Hasura.RQL.Types.Backend (Backend, getAggregationPredicatesModels)
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.RQL.Types.Common (Fields, SourceName, sourceNameToText)
import Hasura.RQL.Types.Relationships.Local
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.StoredProcedure.IR (StoredProcedure (..))
import Language.GraphQL.Draft.Syntax qualified as G

-- Function to get create [ModelInfoPart] type from model names information.
getModelInfoPartfromModelNames :: [ModelNameInfo] -> ModelOperationType -> [ModelInfoPart]
getModelInfoPartfromModelNames modelNames operationType = do
  modelNameInfo <- modelNames
  let (modelName, modelType, sourceName, modelSourceType) = unModelNameInfo modelNameInfo
  pure $ ModelInfoPart modelName modelType (Just $ sourceNameToText sourceName) (Just modelSourceType) operationType

{--
We use this function to get model names from the selection set of a query. E.g., in the following query:
query MyQuery {
  tableA {
    id
    name
    tableATotableBRelationship {
      id
      col
    }
  }
}
The model tableB (since the relationship `tableATotableBRelationship` is pointing calling the table `tableB`) is logged.
--}
getFieldsModelInfoFromAnnFieldGGen ::
  forall a m b.
  (MonadState [ModelNameInfo] m, Backend b) =>
  SourceName ->
  ModelSourceType ->
  AnnFieldG b Void a ->
  m ()
getFieldsModelInfoFromAnnFieldGGen sourceName modelSourceType field = do
  case field of
    AFColumn _col -> do
      return $ ()
    AFArrayRelation arrRel -> do
      case arrRel of
        ASSimple rel -> do
          let arraySelection = _aarAnnSelect rel
          let selectFrom' = _asnFrom arraySelection
          modify $ (++) (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom')
          let arrayFields = _asnFields arraySelection
              args = _saWhere $ _asnArgs arraySelection
              orderBy = _saOrderBy $ _asnArgs arraySelection
          argModelNames <- do
            case args of
              Just arg -> do
                (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
                return res
              Nothing -> pure []
          modify $ (++) argModelNames
          orderByModels <- do
            case orderBy of
              Just orderBy' -> do
                neResult <-
                  for
                    orderBy'
                    ( \orderBy'' -> do
                        (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                        return res
                    )
                -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
                -- relationship, the structure considers each of those checks separately and logs the parent table (to
                -- which the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
                -- any nested relationship model which is same as the parent model).
                pure $ nub $ concat $ NE.toList neResult
              Nothing -> pure []
          modify $ (++) orderByModels
          getFieldsModelNamesGen sourceName modelSourceType arrayFields
        ASAggregate rel -> do
          let arraySelection = _aarAnnSelect rel
          let selectFrom' = _asnFrom arraySelection
          modify $ (++) (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom')
          let arrayFields = _asnFields arraySelection
              args = _saWhere $ _asnArgs arraySelection
              orderBy = _saOrderBy $ _asnArgs arraySelection
          argModelNames <- do
            case args of
              Just arg -> do
                (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
                return res
              Nothing -> pure []
          modify $ (++) argModelNames
          orderByModels <- do
            case orderBy of
              Just orderBy' -> do
                neResult <-
                  for
                    orderBy'
                    ( \orderBy'' -> do
                        (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                        return res
                    )
                -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
                -- relationship, the structure considers each of those checks separately and logs the parent table (to
                -- which the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
                -- any nested relationship model which is same as the parent model).
                pure $ nub $ concat $ NE.toList neResult
              Nothing -> pure []
          modify $ (++) orderByModels
          getAggregateFieldsModelNamesGen sourceName modelSourceType arrayFields
        ASConnection _ -> pure ()
    AFObjectRelation objRel -> do
      let objectSelection = _aarAnnSelect objRel
      let selectFrom' = _aosTarget objectSelection
      modify $ (++) (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom')
      let objFields = _aosFields objectSelection
      getFieldsModelNamesGen sourceName modelSourceType objFields
    AFNestedObject nestedObject -> do
      let fields = _anosFields nestedObject
      getFieldsModelNamesGen sourceName modelSourceType fields
    AFNestedArray _ nestedArray -> do
      case nestedArray of
        ANASSimple fields -> getFieldsModelInfoFromAnnFieldGGen sourceName modelSourceType fields
        ANASAggregate aggregateFields -> do
          let selectFrom' = _asnFrom aggregateFields
          modify $ (++) (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom')
          let fields = _asnFields aggregateFields
              args = _saWhere $ _asnArgs aggregateFields
              orderBy = _saOrderBy $ _asnArgs aggregateFields
          argModelNames <- do
            case args of
              Just arg -> do
                (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
                return res
              Nothing -> pure []
          modify $ (++) argModelNames
          orderByModels <- do
            case orderBy of
              Just orderBy' -> do
                neResult <-
                  for
                    orderBy'
                    ( \orderBy'' -> do
                        (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                        return res
                    )
                -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
                -- relationship, the structure considers each of those checks separately and logs the parent table (to
                -- which the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
                -- any nested relationship model which is same as the parent model).
                pure $ nub $ concat $ NE.toList neResult
              Nothing -> pure []
          modify $ (++) orderByModels
          relModelNames <- do
            (_, res) <- flip runStateT [] $ getAggregateFieldsModelNamesGen sourceName modelSourceType fields
            return res
          modify $ (++) relModelNames
    AFExpression _e -> pure ()
    AFComputedField {} -> pure ()
    AFNodeId {} -> pure ()

-- This function logs the selecton set for a query. This function calls `getFieldsModelInfoFromAnnFieldGGen` which
-- contains the logic to extract the selction set models because the argument is a list type and
-- `getFieldsModelInfoFromAnnFieldGGen` is a recursive function
getFieldsModelNamesGen ::
  forall a m b.
  (MonadState [ModelNameInfo] m, Backend b) =>
  SourceName ->
  ModelSourceType ->
  Fields (AnnFieldG b Void a) ->
  m ()
getFieldsModelNamesGen sourceName modelSourceType fields = for_ fields $ \(_fieldName, field) -> getFieldsModelInfoFromAnnFieldGGen sourceName modelSourceType field

-- This function is similar to `getFieldsModelNamesGen` but it is used for aggregate queries.
getAggregateFieldsModelNamesGen ::
  forall a m b.
  (MonadState [ModelNameInfo] m, Backend b) =>
  SourceName ->
  ModelSourceType ->
  Fields (TableAggregateFieldG b Void a) ->
  m ()
getAggregateFieldsModelNamesGen sourceName modelSourceType fields = for_ fields $ \(_fieldName, field) -> case field of
  TAFNodes _ f -> getFieldsModelNamesGen sourceName modelSourceType f
  _ -> return ()

{-- Core logic function to get model names from the argument of a query. For example, in the following query:
query MyQuery {
  tableA(where: {tableATotableBRelationship: {id: {_lt: 10}}}) {
    id
    name
  }
}
The model tableB (since the relationship `tableATotableBRelationship` is pointing calling the table `tableB`) is logged.
--}

getArgumentModelNamesGen ::
  forall a m b.
  (MonadState [ModelNameInfo] m, Backend b) =>
  SourceName ->
  ModelSourceType ->
  (AnnBoolExp b a) ->
  m ()
getArgumentModelNamesGen sourceName modelSourceType args = case args of
  BoolAnd args' -> for_ args' (getArgumentModelNamesGen sourceName modelSourceType)
  BoolOr args' -> for_ args' (getArgumentModelNamesGen sourceName modelSourceType)
  BoolNot args' -> getArgumentModelNamesGen sourceName modelSourceType args'
  BoolExists g -> do
    getArgumentModelNamesGen sourceName modelSourceType $ _geWhere g
  BoolField field -> case field of
    AVRelationship info rel -> do
      let target = riTarget info
      case target of
        RelTargetTable table -> modify $ (++) [ModelNameInfo (toTxt table, ModelTypeTable, sourceName, modelSourceType)]
        RelTargetNativeQuery q -> modify $ (++) [ModelNameInfo (G.unName $ getNativeQueryName q, ModelTypeNativeQuery, sourceName, modelSourceType)]
      let relFilter = rfFilter rel
      getArgumentModelNamesGen sourceName modelSourceType relFilter
    AVNestedObject field' boolExp -> do
      modify $ (++) [ModelNameInfo (toTxt $ RQL._noiType field', ModelTypeLogicalModels, sourceName, modelSourceType)]
      getArgumentModelNamesGen sourceName modelSourceType boolExp
    AVComputedField computedFieldInfo ->
      modify $ (++) [ModelNameInfo (toTxt $ _acfbName computedFieldInfo, ModelTypeFunction, sourceName, modelSourceType)]
    AVAggregationPredicates aggregationFields -> getAggregationPredicatesModels @b sourceName modelSourceType aggregationFields
    AVColumn {} -> pure ()
    AVRemoteRelationship (RemoteRelPermBoolExp _ _ remoteFetchInfo) -> do
      AB.dispatchAnyBackend @Backend remoteFetchInfo $ \(fetchInfo :: RemoteRelRHSFetchInfo f b') -> do
        let sourceName' = rrrfiSource fetchInfo
            tableName = rrrfiTable fetchInfo
            backendType = reify $ backendTag @b'
            modelSourceType' = case backendType of
              Postgres _ -> ModelSourceTypePostgres
              MSSQL -> ModelSourceTypeMSSQL
              BigQuery -> ModelSourceTypeBigQuery
              DataConnector -> ModelSourceTypeDataConnector
        modify $ (++) [ModelNameInfo (toTxt $ tableName, ModelTypeRemoteSchema, sourceName', modelSourceType')]

{--
This function has the core logic to get the model names from the root field of a query/mutation. For example, in the following:
query MyQuery {
  tableA {
    id
    name
  }
}
The model tableA is logged.
--}
modelInfoFromClauseGen ::
  forall b a.
  (Backend b) =>
  SourceName ->
  ModelSourceType ->
  SelectFromG b a ->
  Maybe (Text, ModelType, SourceName, ModelSourceType)
modelInfoFromClauseGen sourceName modelSourceType selectFrom = case selectFrom of
  FromTable table -> Just (toTxt table, ModelTypeTable, sourceName, modelSourceType)
  FromNativeQuery nquery -> Just (toTxt $ getLogicalModelName $ lmName $ nqLogicalModel nquery, ModelTypeLogicalModels, sourceName, modelSourceType)
  FromFunction functionName _ _ ->
    -- `jsonb_to_recordset` to query the remote relationship in Postgres, which logs extra model log for a remote relationship.
    if (toTxt functionName == "pg_catalog.jsonb_to_recordset")
      then Nothing
      else Just (toTxt functionName, ModelTypeFunction, sourceName, modelSourceType)
  FromIdentifier identifier -> Just (unFIIdentifier identifier, ModelTypeIdentifier, sourceName, modelSourceType)
  FromStoredProcedure storedProcedure -> Just (G.unName $ spGraphqlName storedProcedure, ModelTypeStoredProcedures, sourceName, modelSourceType)

-- This function is a wrapper around `modelInfoFromClauseGen` to get the model names from the root field of a
-- query/mutation. We can also change the type of `modelInfoFromClauseGen` to `[ModelNameInfo]` and deal with the filter
-- there only.
getModelInfoListFromModelInfoClauseGen ::
  forall b a.
  (Backend b) =>
  SourceName ->
  ModelSourceType ->
  SelectFromG b a ->
  [ModelNameInfo]
getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom = do
  case modelInfoFromClauseGen sourceName modelSourceType selectFrom of
    Just modelInfo -> [ModelNameInfo modelInfo]
    Nothing -> []

{--
This function has the core logic to get the model names from the orderby clause of a query. For example, in the following:
query MyQuery {
  tableA(order_by: {tableATotableBRelationship: {id: asc}}) {
    id
    name
  }
}
The model tableB (since the relationship `tableATotableBRelationship` is pointing calling the table `tableB`) is logged.
--}
getOrderByModels ::
  forall a m b.
  (MonadState [ModelNameInfo] m, Backend b) =>
  SourceName ->
  ModelSourceType ->
  (AnnotatedOrderByElement b a) ->
  m ()
getOrderByModels sourceName modelSourceType orderBy = do
  case orderBy of
    AOCColumn _ _ -> pure ()
    AOCObjectRelation relInfo constraint nestedValue -> do
      let selectFrom' = riTarget relInfo
      case selectFrom' of
        RelTargetTable table -> modify $ (++) [ModelNameInfo (toTxt table, ModelTypeTable, sourceName, modelSourceType)]
        RelTargetNativeQuery q -> modify $ (++) [ModelNameInfo (G.unName $ getNativeQueryName q, ModelTypeNativeQuery, sourceName, modelSourceType)]
      getArgumentModelNamesGen sourceName modelSourceType constraint
      getOrderByModels sourceName modelSourceType nestedValue
    AOCNestedObject info nestedVal -> do
      modify $ (++) [ModelNameInfo (toTxt $ RQL._noiType info, ModelTypeLogicalModels, sourceName, modelSourceType)]
      getOrderByModels sourceName modelSourceType nestedVal
    AOCArrayAggregation relInfo constraint _ -> do
      let selectFrom' = riTarget relInfo
      case selectFrom' of
        RelTargetTable table -> modify $ (++) [ModelNameInfo (toTxt table, ModelTypeTable, sourceName, modelSourceType)]
        RelTargetNativeQuery q -> modify $ (++) [ModelNameInfo (G.unName $ getNativeQueryName q, ModelTypeNativeQuery, sourceName, modelSourceType)]
      getArgumentModelNamesGen sourceName modelSourceType constraint
    AOCComputedField computedFieldInfo -> do
      modify $ (++) [ModelNameInfo (toTxt $ _cfobName computedFieldInfo, ModelTypeFunction, sourceName, modelSourceType)]
  pure ()

-- convert a query from an intermediate representation to model info. This function contains the logic to get all the
-- models for a query/subscription
irToModelInfoGen ::
  forall b m a.
  ( Backend b,
    Monad m
  ) =>
  SourceName ->
  ModelSourceType ->
  QueryDB b Void a ->
  m [ModelNameInfo]
irToModelInfoGen sourceName modelSourceType = \case
  QDBMultipleRows s -> do
    let selectFrom = _asnFrom s
    let fields = _asnFields s
    let args = _saWhere $ _asnArgs s
    let orderBy = _saOrderBy $ _asnArgs s
    relModelNames <- do
      (_, res) <- flip runStateT [] $ getFieldsModelNamesGen sourceName modelSourceType fields
      return res
    argModelNames <- do
      case args of
        Just arg -> do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
          return res
        Nothing -> return []
    permissionModelNames <- do
      let permissionFilter = _tpFilter $ _asnPerm s
      (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType permissionFilter
      return res
    orderByModels <- do
      case orderBy of
        Just orderBy' -> do
          neResult <-
            for
              orderBy'
              ( \orderBy'' -> do
                  (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                  return res
              )
          -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
          -- relationship, the structure considers each of those checks separately and logs the parent table (to which
          -- the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
          -- any nested relationship model which is same as the parent model).
          pure $ nub $ concat $ NE.toList neResult
        Nothing -> pure []

    let modelNames =
          relModelNames
            <> argModelNames
            <> (orderByModels)
            <> (permissionModelNames)
            <> (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom)

    return $ modelNames
  QDBSingleRow s -> do
    let selectFrom = _asnFrom s
    let fields = _asnFields s
    let args = _saWhere $ _asnArgs s
    let orderBy = _saOrderBy $ _asnArgs s
    relModelNames <- do
      (_, res) <- flip runStateT [] $ getFieldsModelNamesGen sourceName modelSourceType fields
      return res
    argModelNames <- do
      case args of
        Just arg -> do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
          return res
        Nothing -> return []
    permissionModelNames <- do
      let permissionFilter = _tpFilter $ _asnPerm s
      (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType permissionFilter
      return res
    orderByModels <- do
      case orderBy of
        Just orderBy' -> do
          neResult <-
            for
              orderBy'
              ( \orderBy'' -> do
                  (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                  return res
              )
          -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
          -- relationship, the structure considers each of those checks separately and logs the parent table (to which
          -- the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
          -- any nested relationship model which is same as the parent model).
          pure $ nub $ concat $ NE.toList neResult
        Nothing -> pure []
    let modelNames =
          relModelNames
            <> argModelNames
            <> orderByModels
            <> (permissionModelNames)
            <> ((getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom))
    return $ (modelNames)
  QDBAggregation s -> do
    let selectFrom = _asnFrom s
    let fields = _asnFields s
    let args = _saWhere $ _asnArgs s
    let orderBy = _saOrderBy $ _asnArgs s
    relModelNames <- do
      (_, res) <- flip runStateT [] $ getAggregateFieldsModelNamesGen sourceName modelSourceType fields
      return res
    argModelNames <- do
      case args of
        Just arg -> do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
          return res
        Nothing -> return []
    permissionModelNames <- do
      let permissionFilter = _tpFilter $ _asnPerm s
      (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType permissionFilter
      return res
    orderByModels <- do
      case orderBy of
        Just orderBy' -> do
          neResult <-
            for
              orderBy'
              ( \orderBy'' -> do
                  (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                  return res
              )
          -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
          -- relationship, the structure considers each of those checks separately and logs the parent table (to which
          -- the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
          -- any nested relationship model which is same as the parent model).
          pure $ nub $ concat $ NE.toList neResult
        Nothing -> pure []
    let modelNames =
          relModelNames
            <> argModelNames
            <> orderByModels
            <> permissionModelNames
            <> (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom)
    return $ (modelNames)
  QDBConnection _ -> do
    return []
  QDBStreamMultipleRows s -> do
    let selectFrom = _assnFrom s
    let fields = _assnFields s
    let args = _ssaWhere $ _assnArgs s
    relModelNames <- do
      (_, res) <- flip runStateT [] $ getFieldsModelNamesGen sourceName modelSourceType fields
      return res
    argModelNames <- do
      case args of
        Just arg -> do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
          return res
        Nothing -> return []
    permissionModelNames <- do
      let permissionFilter = _tpFilter $ _assnPerm s
      (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType permissionFilter
      return res

    let modelNames =
          relModelNames
            <> argModelNames
            <> permissionModelNames
            <> (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom)
    return $ modelNames

-- This function is a wrapper around `getFieldsModelNamesGen` to get the model names from the selection set of a
-- mutation.
getMutationOutputModelNamesGen ::
  forall b a.
  (Backend b) =>
  SourceName ->
  ModelSourceType ->
  MutationOutputG b Void a ->
  [ModelNameInfo]
getMutationOutputModelNamesGen sourceName modelSourceType outputMut = do
  case outputMut of
    MOutSinglerowObject fields -> do
      join do
        (_, res') <- flip runStateT [] $ getFieldsModelNamesGen sourceName modelSourceType fields
        return res'
    MOutMultirowFields multFs -> do
      let modelInfoList =
            map
              ( \(fieldName, fields) -> do
                  let _f = fieldName
                  case fields of
                    MRet field -> do
                      join do
                        (_, res') <- flip runStateT [] $ getFieldsModelNamesGen sourceName modelSourceType field
                        return res'
                    _ -> []
              )
              multFs
      concat modelInfoList

-- This function is used to get the model names from the remote relationships in MSSQL and DataConnector.
getRSModelInfoGen ::
  forall b a m.
  (Monad m, Backend b) =>
  SourceName ->
  ModelSourceType ->
  SourceRelationshipSelection b Void a ->
  m [ModelNameInfo]
getRSModelInfoGen sourceName modelSourceType ir = case ir of
  SourceRelationshipObject objectSelect -> do
    let selectFrom' = _aosTarget objectSelect
    let objFields = _aosFields objectSelect
    (_, fieldModelRes) <- flip runStateT [] $ getFieldsModelNamesGen sourceName modelSourceType objFields
    let argFilter = _aosTargetFilter objectSelect
    (_, argModelRes) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType argFilter
    let modelNames =
          (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom')
            <> argModelRes
            <> fieldModelRes
    pure modelNames
  SourceRelationshipArray simpleSelect -> do
    let selectFrom = _asnFrom simpleSelect
    let fields = _asnFields simpleSelect
    let args = _saWhere $ _asnArgs simpleSelect
    let orderBy = _saOrderBy $ _asnArgs simpleSelect
    relModelNames <- do
      (_, res) <- flip runStateT [] $ getFieldsModelNamesGen sourceName modelSourceType fields
      return res
    argModelNames <- do
      case args of
        Just arg -> do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
          return res
        Nothing -> return []
    permissionModelNames <- do
      let permissionFilter = _tpFilter $ _asnPerm simpleSelect
      (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType permissionFilter
      return res
    orderByModels <- do
      case orderBy of
        Just orderBy' -> do
          neResult <-
            for
              orderBy'
              ( \orderBy'' -> do
                  (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                  return res
              )
          -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
          -- relationship, the structure considers each of those checks separately and logs the parent table (to which
          -- the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
          -- any nested relationship model which is same as the parent model).
          pure $ nub $ concat $ NE.toList neResult
        Nothing -> pure []
    let modelNames =
          relModelNames
            <> argModelNames
            <> orderByModels
            <> permissionModelNames
            <> (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom)
    pure modelNames
  SourceRelationshipArrayAggregate aggregateSelect -> do
    let selectFrom = _asnFrom aggregateSelect
    let fields = _asnFields aggregateSelect
    let args = _saWhere $ _asnArgs aggregateSelect
    let orderBy = _saOrderBy $ _asnArgs aggregateSelect
    relModelNames <- do
      (_, res) <- flip runStateT [] $ getAggregateFieldsModelNamesGen sourceName modelSourceType fields
      return res
    argModelNames <- do
      case args of
        Just arg -> do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType arg
          return res
        Nothing -> return []
    permissionModelNames <- do
      let permissionFilter = _tpFilter $ _asnPerm aggregateSelect
      (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType permissionFilter
      return res
    orderByModels <- do
      case orderBy of
        Just orderBy' -> do
          neResult <-
            for
              orderBy'
              ( \orderBy'' -> do
                  (_, res) <- flip runStateT [] $ getOrderByModels sourceName modelSourceType (obiColumn orderBy'')
                  return res
              )
          -- Note: when using orderby, if there is a relationship and there are multiple checks for orderby in the
          -- relationship, the structure considers each of those checks separately and logs the parent table (to which
          -- the RHS of the join). Using `nub` removes the duplicate model logs (although, it will also not log
          -- any nested relationship model which is same as the parent model).
          pure $ nub $ concat $ NE.toList neResult
        Nothing -> pure []
    let modelNames =
          relModelNames
            <> argModelNames
            <> orderByModels
            <> permissionModelNames
            <> (getModelInfoListFromModelInfoClauseGen sourceName modelSourceType selectFrom)
    pure $ modelNames

{--
This function has the core logic to get the model names from the argument (and on-conflict where clause) of a mutation for Postgres databases. For example, in the following:
mutation MyMutation {
  insert_tableA(object: {tableATotableBRelationship: {data: {id: 1}}}) {
    id
    name
  }
}
The model tableB (since the relationship `tableATotableBRelationship` is pointing calling the table `tableB`) is logged.
--}
getMutationInsertArgumentModelNamesPostgres ::
  forall f m pgKind.
  (Traversable f, MonadState [ModelNameInfo] m, Backend ('Postgres pgKind)) =>
  SourceName ->
  ModelSourceType ->
  AnnotatedInsertData ('Postgres pgKind) f (UnpreparedValue ('Postgres pgKind)) ->
  m ()
getMutationInsertArgumentModelNamesPostgres sourceName modelSourceType insertOperation = do
  let (modelName, modelType) = (qualifiedObjectToText (_aiTableName $ insertOperation), ModelTypeTable)
  modify $ (++) [ModelNameInfo (modelName, modelType, sourceName, modelSourceType)]
  let insertFields = _aiInsertObject insertOperation
  for_
    insertFields
    ( \annotatedFieldsList ->
        for_
          annotatedFieldsList
          ( \annotatedField -> do
              case annotatedField of
                AIColumn _ -> pure ()
                AIObjectRelationship _ objRel -> getMutationInsertArgumentModelNamesPostgres sourceName modelSourceType $ _riInsertData objRel
                AIArrayRelationship _ arrRel -> getMutationInsertArgumentModelNamesPostgres sourceName modelSourceType $ _riInsertData arrRel
          )
    )

  let onConflictClause = _biConflictClause $ _aiBackendInsert insertOperation
  case onConflictClause of
    Nothing -> pure ()
    Just conflict -> do
      case conflict of
        OCCDoNothing _doNothing -> do
          pure ()
        OCCUpdate whereClause -> do
          let argModelBoolExp = cp1udFilter whereClause
          (_, res') <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType argModelBoolExp
          modify $ (++) res'

-- Similar to `getMutationInsertArgumentModelNamesPostgres` but for MSSQL
getMutationInsertArgumentModelNamesMSSQL ::
  forall f m.
  (MonadState [ModelNameInfo] m) =>
  SourceName ->
  ModelSourceType ->
  AnnotatedInsertData 'MSSQL f (UnpreparedValue 'MSSQL) ->
  m ()
getMutationInsertArgumentModelNamesMSSQL sourceName modelSourceType insertOperation = do
  let (modelName, modelType) = (MSSQL.tableName (_aiTableName $ insertOperation), ModelTypeTable)
  modify $ (++) [ModelNameInfo (modelName, modelType, sourceName, modelSourceType)]

  let onConflictClause = MSSQL._biIfMatched $ _aiBackendInsert $ insertOperation
  case onConflictClause of
    Nothing -> pure ()
    Just conflict -> do
      let argModelBoolExp = MSSQL._imConditions conflict
      (_, res') <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType $ argModelBoolExp
      modify $ (++) res'

-- Similar to `getMutationInsertArgumentModelNamesPostgres` but for DataConnector. Note: DataConnector doesn't have any
-- conflict clause.
getMutationInsertArgumentModelNamesDC ::
  forall f m.
  (MonadState [ModelNameInfo] m) =>
  SourceName ->
  ModelSourceType ->
  AnnotatedInsertData 'DataConnector f (UnpreparedValue 'DataConnector) ->
  m ()
getMutationInsertArgumentModelNamesDC sourceName modelSourceType insertOperation = do
  let (modelName, modelType) = (toTxt (_aiTableName $ insertOperation), ModelTypeTable)
  modify $ (++) [ModelNameInfo (modelName, modelType, sourceName, modelSourceType)]
