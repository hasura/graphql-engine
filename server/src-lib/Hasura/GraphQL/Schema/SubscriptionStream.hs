{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate the GraphQL schema types related to streaming subscriptions.
module Hasura.GraphQL.Schema.SubscriptionStream
  ( selectStreamTable,
  )
where

import Control.Lens ((^?))
import Control.Monad.Memoize
import Data.Has
import Data.List.NonEmpty qualified as NE
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Extended ((<>>))
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp (AggregationPredicatesSchema)
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select (tableSelectionList, tableWhereArg)
import Hasura.GraphQL.Schema.Table (getTableGQLName, getTableIdentifierName, tableSelectColumns, tableSelectPermissions)
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Subscription
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | Argument to limit the maximum number of results returned in a single batch.
cursorBatchSizeArg ::
  forall n.
  (MonadParse n) =>
  NamingCase ->
  InputFieldsParser n Int
cursorBatchSizeArg tCase =
  fromIntegral
    <$> P.field batchSizeName batchSizeDesc P.nonNegativeInt
  where
    batchSizeName = applyFieldNameCaseCust tCase Name._batch_size
    batchSizeDesc = Just $ G.Description "maximum number of rows returned in a single batch"

-- | Cursor ordering enum fields
--
-- > enum cursor_ordering {
-- >   ASC
-- >   DESC
-- > }
cursorOrderingArgParser ::
  forall b r m n.
  (MonadBuildSourceSchema b r m n) =>
  SchemaT r m (Parser 'Both n CursorOrdering)
cursorOrderingArgParser = do
  sourceInfo :: SourceInfo b <- asks getter
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      enumName = runMkTypename (_rscTypeNames customization) $ applyTypeNameCaseCust tCase Name._cursor_ordering
  let description =
        Just
          $ G.Description
          $ "ordering argument of a cursor"
  pure
    $ P.enum enumName description
    $ NE.fromList -- It's fine to use fromList here because we know the list is never empty.
      [ ( define enumNameVal,
          snd enumNameVal
        )
        | enumNameVal <- [(Name._ASC, COAscending), (Name._DESC, CODescending)]
      ]
  where
    define (name, val) =
      let orderingTypeDesc = bool "descending" "ascending" $ val == COAscending
       in P.Definition name (Just $ G.Description $ orderingTypeDesc <> " ordering of the cursor") Nothing [] P.EnumValueInfo

-- | Argument to specify the ordering of the cursor.
-- > ordering: cursor_ordering
cursorOrderingArg ::
  forall b r m n.
  (MonadBuildSourceSchema b r m n) =>
  SchemaT r m (InputFieldsParser n (Maybe CursorOrdering))
cursorOrderingArg = do
  cursorOrderingParser' <- cursorOrderingArgParser @b
  pure $ P.fieldOptional Name._ordering (Just $ G.Description "cursor ordering") cursorOrderingParser'

-- | Input fields parser to parse the value of a table's column
-- > column_name: column_type
streamColumnParserArg ::
  forall b n m r.
  (MonadBuildSchema b r m n) =>
  (ColumnInfo b, IR.AnnRedactionExpUnpreparedValue b) ->
  SchemaT r m (InputFieldsParser n (Maybe (ColumnInfo b, IR.AnnRedactionExpUnpreparedValue b, ColumnValue b)))
streamColumnParserArg (colInfo, redactionExp) = do
  fieldParser <- typedParser colInfo
  let fieldName = ciName colInfo
      fieldDesc = ciDescription colInfo
  pure do
    P.fieldOptional fieldName fieldDesc fieldParser <&> fmap (colInfo,redactionExp,)
  where
    typedParser columnInfo = do
      fmap IR.openValueOrigin <$> columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)

-- | Input object parser whose keys are the column names and the values are the
--   initial values of those columns from where the streaming should start.
-- > input table_stream_cursor_value_input {
-- >   col1: col1_type
-- >   col2: col2_type
--     ...
-- > }
streamColumnValueParser ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  GQLNameIdentifier ->
  NE.NonEmpty (ColumnInfo b, IR.AnnRedactionExpUnpreparedValue b) ->
  SchemaT r m (Parser 'Input n [(ColumnInfo b, IR.AnnRedactionExpUnpreparedValue b, ColumnValue b)])
streamColumnValueParser tableGQLIdentifier colInfos = do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      objName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkStreamCursorValueInputTypeName tableGQLIdentifier
      description = G.Description $ "Initial value of the column from where the streaming should start"
  memoizeOn 'streamColumnValueParser (sourceName, tableGQLIdentifier) $ do
    columnVals <- sequenceA <$> traverse streamColumnParserArg colInfos
    pure $ P.object objName (Just description) columnVals <&> (catMaybes . NE.toList)

-- | Argument to accept the initial value from where the streaming should start.
-- > initial_value: table_stream_cursor_value_input!
streamColumnValueParserArg ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  GQLNameIdentifier ->
  NE.NonEmpty (ColumnInfo b, IR.AnnRedactionExpUnpreparedValue b) ->
  SchemaT r m (InputFieldsParser n [(ColumnInfo b, IR.AnnRedactionExpUnpreparedValue b, ColumnValue b)])
streamColumnValueParserArg tableGQLIdentifier nonEmptyColInfos = do
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b
  columnValueParser <- streamColumnValueParser tableGQLIdentifier nonEmptyColInfos
  pure do
    P.field (applyFieldNameCaseCust tCase Name._initial_value) (Just $ G.Description "Stream column input with initial value") columnValueParser

-- | Argument to accept the cursor data. At the time of writing this, only a single
--   column cursor is supported and if multiple column cursors are provided,
--   then a parse error is thrown.
-- >
tableStreamColumnArg ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  GQLNameIdentifier ->
  NE.NonEmpty (ColumnInfo b, IR.AnnRedactionExpUnpreparedValue b) ->
  SchemaT r m (InputFieldsParser n [IR.StreamCursorItem b (IR.UnpreparedValue b)])
tableStreamColumnArg tableGQLIdentifier colInfos = do
  cursorOrderingParser <- cursorOrderingArg @b
  streamColumnParser <- streamColumnValueParserArg tableGQLIdentifier colInfos
  pure $ do
    orderingArg <- cursorOrderingParser
    columnArg <- streamColumnParser
    pure $ (\(columnInfo, redactionExp, columnValue) -> IR.StreamCursorItem (fromMaybe COAscending orderingArg) columnInfo redactionExp columnValue) <$> columnArg

-- | Input object that contains the initial value of a column
--   along with how it needs to be ordered.
-- > input table_stream_cursor_input {
-- >   initial_value: table_stream_cursor_value_input!
-- >   ordering: cursor_ordering
-- > }
tableStreamCursorExp ::
  forall m n r b.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Input n [(IR.StreamCursorItem b (IR.UnpreparedValue b))]))
tableStreamCursorExp tableInfo = runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  tableGQLName <- getTableGQLName tableInfo
  tableGQLIdentifier <- getTableIdentifierName tableInfo
  columnInfos <- mapMaybe (\(column, redactionExp) -> (,redactionExp) <$> column ^? _SCIScalarColumn) <$> tableSelectColumns tableInfo
  columnInfosNE <- hoistMaybe $ NE.nonEmpty columnInfos
  lift $ memoizeOn 'tableStreamCursorExp (sourceName, tableName) do
    columnParsers <- tableStreamColumnArg tableGQLIdentifier columnInfosNE
    let objName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkStreamCursorInputTypeName tableGQLIdentifier
        description = G.Description $ "Streaming cursor of the table " <>> tableGQLName
    pure $ P.object objName (Just description) columnParsers

-- | Argument to accept the cursor input object.
-- > cursor: [table_stream_cursor_input]!
tableStreamCursorArg ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Maybe (InputFieldsParser n [IR.StreamCursorItem b (IR.UnpreparedValue b)]))
tableStreamCursorArg tableInfo = runMaybeT do
  cursorParser <- MaybeT $ tableStreamCursorExp tableInfo
  pure $ do
    cursorArgs <-
      P.field cursorName cursorDesc $ P.list $ P.nullable cursorParser
    pure $ concat $ catMaybes cursorArgs
  where
    cursorName = Name._cursor
    cursorDesc = Just $ G.Description "cursor to stream the results returned by the query"

-- | Arguments to the streaming subscription field.
-- > table_stream (cursor: [table_stream_cursor_input]!, batch_size: Int!, where: table_bool_exp)
tableStreamArguments ::
  forall b r m n.
  ( AggregationPredicatesSchema b,
    MonadBuildSchema b r m n
  ) =>
  TableInfo b ->
  SchemaT r m (Maybe (InputFieldsParser n (SelectStreamArgs b)))
tableStreamArguments tableInfo = runMaybeT $ do
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b
  whereParser <- lift $ tableWhereArg tableInfo
  cursorParser <- MaybeT $ tableStreamCursorArg tableInfo
  pure $ do
    whereArg <- whereParser
    cursorArg <-
      cursorParser `P.bindFields` \case
        [] -> parseError "one streaming column field is expected"
        [c] -> pure c
        _ -> parseError "multiple column cursors are not supported yet"
    batchSizeArg <- cursorBatchSizeArg tCase
    pure
      $ IR.SelectStreamArgsG whereArg batchSizeArg cursorArg

-- | Field parser for a streaming subscription for a table.
selectStreamTable ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b
  ) =>
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (P.FieldParser n (StreamSelectExp b)))
selectStreamTable tableInfo fieldName description = runMaybeT $ do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
  roleName <- retrieve scRole
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  xStreamSubscription <- hoistMaybe $ streamSubscriptionExtension @b
  stringifyNumbers <- retrieve Options.soStringifyNumbers
  tableStreamArgsParser <- MaybeT $ tableStreamArguments tableInfo
  selectionSetParser <- MaybeT $ tableSelectionList tableInfo
  lift
    $ memoizeOn 'selectStreamTable (sourceName, tableName, fieldName)
    $ do
      pure
        $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
        $ P.subselection fieldName description tableStreamArgsParser selectionSetParser
        <&> \(args, fields) ->
          IR.AnnSelectStreamG
            { IR._assnXStreamingSubscription = xStreamSubscription,
              IR._assnFields = fields,
              IR._assnFrom = IR.FromTable tableName,
              IR._assnPerm = tablePermissionsInfo selectPermissions,
              IR._assnArgs = args,
              IR._assnStrfyNum = stringifyNumbers
            }
