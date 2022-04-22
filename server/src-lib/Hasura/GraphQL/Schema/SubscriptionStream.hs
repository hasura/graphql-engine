{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate the GraphQL schema types related to streaming subscriptions.
module Hasura.GraphQL.Schema.SubscriptionStream
  ( selectStreamTable,
  )
where

import Data.Has
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended ((<>>))
import Hasura.Base.Error (QErr)
import Hasura.GraphQL.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Select (tablePermissionsInfo, tableSelectionList, tableWhereArg)
import Hasura.GraphQL.Schema.Table (getTableGQLName, tableSelectColumns, tableSelectPermissions)
import Hasura.Prelude
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

-- | Argument to limit the maximum number of results returned in a single batch.
cursorBatchSizeArg ::
  forall n.
  MonadParse n =>
  InputFieldsParser n Int
cursorBatchSizeArg =
  fromIntegral
    <$> P.field batchSizeName batchSizeDesc P.nonNegativeInt
  where
    batchSizeName = G._batch_size
    batchSizeDesc = Just $ G.Description "maximum number of rows returned in a single batch"

-- | Cursor ordering enum fields
--
-- > enum cursor_ordering {
-- >   ASC
-- >   DESC
-- > }
cursorOrderingArgParser ::
  forall n m r.
  (MonadSchema n m, Has P.MkTypename r, MonadReader r m) =>
  m (Parser 'Both n CursorOrdering)
cursorOrderingArgParser = do
  enumName <- P.mkTypename G._cursor_ordering
  let description =
        Just $
          G.Description $
            "ordering argument of a cursor"
  pure $
    P.enum enumName description $
      NE.fromList -- It's fine to use fromList here because we know the list is never empty.
        [ ( define enumNameVal,
            snd enumNameVal
          )
          | enumNameVal <- [(G._ASC, COAscending), (G._DESC, CODescending)]
        ]
  where
    define (name, val) =
      let orderingTypeDesc = bool "descending" "ascending" $ val == COAscending
       in P.Definition name (Just $ G.Description $ orderingTypeDesc <> " ordering of the cursor") P.EnumValueInfo

-- | Argument to specify the ordering of the cursor.
-- > ordering: cursor_ordering
cursorOrderingArg ::
  forall n m r.
  (MonadSchema n m, Has P.MkTypename r, MonadReader r m) =>
  m (InputFieldsParser n (Maybe CursorOrdering))
cursorOrderingArg = do
  cursorOrderingParser' <- cursorOrderingArgParser
  pure do
    P.fieldOptional G._ordering (Just $ G.Description "cursor ordering") cursorOrderingParser'

-- | Input fields parser to parse the value of a table's column
-- > column_name: column_type
streamColumnParserArg ::
  forall b n m r.
  (BackendSchema b, MonadSchema n m, Has P.MkTypename r, MonadReader r m, MonadError QErr m) =>
  ColumnInfo b ->
  m (InputFieldsParser n (Maybe (ColumnInfo b, ColumnValue b)))
streamColumnParserArg colInfo = do
  fieldParser <- typedParser colInfo
  let fieldName = ciName colInfo
      fieldDesc = ciDescription colInfo
  pure do
    P.fieldOptional fieldName fieldDesc fieldParser <&> fmap (colInfo,)
  where
    typedParser columnInfo = do
      fmap P.openValueOrigin <$> columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)

-- | Input object parser whose keys are the column names and the values are the
--   initial values of those columns from where the streaming should start.
-- > input table_stream_cursor_value_input {
-- >   col1: col1_type
-- >   col2: col2_type
--     ...
-- > }
streamColumnValueParser ::
  forall b n m r.
  (BackendSchema b, MonadSchema n m, Has P.MkTypename r, MonadReader r m, MonadError QErr m) =>
  SourceName ->
  G.Name ->
  [ColumnInfo b] ->
  m (Parser 'Input n [(ColumnInfo b, ColumnValue b)])
streamColumnValueParser sourceName tableGQLName colInfos =
  memoizeOn 'streamColumnValueParser (sourceName, tableGQLName) $ do
    columnVals <- sequenceA <$> traverse streamColumnParserArg colInfos
    objName <- P.mkTypename $ tableGQLName <> G.__stream_cursor_value_input
    pure do
      let description = G.Description $ "Initial value of the column from where the streaming should start"
      P.object objName (Just description) columnVals <&> catMaybes

-- | Argument to accept the initial value from where the streaming should start.
-- > initial_value: table_stream_cursor_value_input!
streamColumnValueParserArg ::
  forall b n m r.
  ( BackendSchema b,
    MonadSchema n m,
    Has P.MkTypename r,
    MonadReader r m,
    MonadError QErr m
  ) =>
  SourceName ->
  G.Name ->
  [ColumnInfo b] ->
  m (InputFieldsParser n [(ColumnInfo b, ColumnValue b)])
streamColumnValueParserArg sourceName tableGQLName colInfos = do
  columnValueParser <- streamColumnValueParser sourceName tableGQLName colInfos
  pure do
    P.field G._initial_value (Just $ G.Description "Stream column input with initial value") columnValueParser

-- | Argument to accept the cursor data. At the time of writing this, only a single
--   column cursor is supported and if multiple column cursors are provided,
--   then a parse error is thrown.
-- >
tableStreamColumnArg ::
  forall n m r b.
  (BackendSchema b, MonadSchema n m, Has P.MkTypename r, MonadReader r m, MonadError QErr m) =>
  SourceName ->
  G.Name ->
  [ColumnInfo b] ->
  m (InputFieldsParser n [IR.StreamCursorItem b])
tableStreamColumnArg sourceName tableGQLName colInfos = do
  cursorOrderingParser <- cursorOrderingArg
  streamColumnParser <- streamColumnValueParserArg sourceName tableGQLName colInfos
  pure $ do
    orderingArg <- cursorOrderingParser
    columnArg <- streamColumnParser
    pure $ (uncurry (IR.StreamCursorItem (fromMaybe COAscending orderingArg))) <$> columnArg

-- | Input object that contains the initial value of a column
--   along with how it needs to be ordered.
-- > input table_stream_cursor_input {
-- >   initial_value: table_stream_cursor_value_input!
-- >   ordering: cursor_ordering
-- > }
tableStreamCursorExp ::
  forall m n r b.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  m (Parser 'Input n [(IR.StreamCursorItem b)])
tableStreamCursorExp sourceName tableInfo =
  memoizeOn 'tableStreamCursorExp (sourceName, tableInfoName tableInfo) $ do
    tableGQLName <- getTableGQLName tableInfo
    columnInfos <- tableSelectColumns sourceName tableInfo
    objName <- P.mkTypename $ tableGQLName <> G.__stream_cursor_input
    let description =
          G.Description $ "Streaming cursor of the table " <>> tableGQLName
    columnParsers <- tableStreamColumnArg sourceName tableGQLName columnInfos
    pure $ P.object objName (Just description) columnParsers

-- | Argument to accept the cursor input object.
-- > cursor: [table_stream_cursor_input]!
tableStreamCursorArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  m (InputFieldsParser n [IR.StreamCursorItem b])
tableStreamCursorArg sourceName tableInfo = do
  cursorParser <- tableStreamCursorExp sourceName tableInfo
  pure $ do
    cursorArgs <-
      P.field cursorName cursorDesc $ P.list $ P.nullable cursorParser
    pure $ concat $ catMaybes cursorArgs
  where
    cursorName = G._cursor
    cursorDesc = Just $ G.Description "cursor to stream the results returned by the query"

-- | Arguments to the streaming subscription field.
-- > table_stream (cursor: [table_stream_cursor_input]!, batch_size: Int!, where: table_bool_exp)
tableStreamArguments ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  m (InputFieldsParser n (SelectStreamArgs b))
tableStreamArguments sourceName tableInfo = do
  whereParser <- tableWhereArg sourceName tableInfo
  cursorParser <- tableStreamCursorArg sourceName tableInfo
  pure $ do
    whereArg <- whereParser
    cursorArg <-
      cursorParser `P.bindFields` \case
        [] -> parseError "one streaming column field is expected"
        [c] -> pure c
        _ -> parseError "multiple column cursors are not supported yet"
    batchSizeArg <- cursorBatchSizeArg
    pure $
      IR.SelectStreamArgsG whereArg batchSizeArg cursorArg

-- | Field parser for a streaming subscription for a table.
selectStreamTable ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  m (Maybe (P.FieldParser n (StreamSelectExp b)))
selectStreamTable sourceName tableInfo fieldName description = runMaybeT $ do
  selectPermissions <- MaybeT $ tableSelectPermissions tableInfo
  xStreamSubscription <- hoistMaybe $ streamSubscriptionExtension @b
  stringifyNum <- asks $ qcStringifyNum . getter
  tableStreamArgsParser <- lift $ tableStreamArguments sourceName tableInfo
  selectionSetParser <- MaybeT $ tableSelectionList sourceName tableInfo
  lift $
    memoizeOn 'selectStreamTable (sourceName, tableName, fieldName) $ do
      pure $
        P.subselection fieldName description tableStreamArgsParser selectionSetParser
          <&> \(args, fields) ->
            IR.AnnSelectStreamG
              { IR._assnXStreamingSubscription = xStreamSubscription,
                IR._assnFields = fields,
                IR._assnFrom = IR.FromTable tableName,
                IR._assnPerm = tablePermissionsInfo selectPermissions,
                IR._assnArgs = args,
                IR._assnStrfyNum = stringifyNum
              }
  where
    tableName = tableInfoName tableInfo
