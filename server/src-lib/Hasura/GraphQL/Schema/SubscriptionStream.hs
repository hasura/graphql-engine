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
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select (tablePermissionsInfo, tableSelectionList, tableWhereArg)
import Hasura.GraphQL.Schema.Table (getTableGQLName, tableSelectColumns, tableSelectPermissions)
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization (applyFieldNameCaseCust, applyTypeNameCaseCust)
import Hasura.RQL.Types.Subscription
import Hasura.RQL.Types.Table
import Language.GraphQL.Draft.Syntax qualified as G

-- | Argument to limit the maximum number of results returned in a single batch.
cursorBatchSizeArg ::
  forall n.
  MonadParse n =>
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
  forall n m r.
  (MonadSchema n m, Has MkTypename r, Has NamingCase r, MonadReader r m) =>
  m (Parser 'Both n CursorOrdering)
cursorOrderingArgParser = do
  tCase <- asks getter
  enumName <- mkTypename $ applyTypeNameCaseCust tCase Name._cursor_ordering
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
          | enumNameVal <- [(Name._ASC, COAscending), (Name._DESC, CODescending)]
        ]
  where
    define (name, val) =
      let orderingTypeDesc = bool "descending" "ascending" $ val == COAscending
       in P.Definition name (Just $ G.Description $ orderingTypeDesc <> " ordering of the cursor") Nothing P.EnumValueInfo

-- | Argument to specify the ordering of the cursor.
-- > ordering: cursor_ordering
cursorOrderingArg ::
  forall n m r.
  (MonadSchema n m, Has MkTypename r, Has NamingCase r, MonadReader r m) =>
  m (InputFieldsParser n (Maybe CursorOrdering))
cursorOrderingArg = do
  cursorOrderingParser' <- cursorOrderingArgParser
  pure do
    P.fieldOptional Name._ordering (Just $ G.Description "cursor ordering") cursorOrderingParser'

-- | Input fields parser to parse the value of a table's column
-- > column_name: column_type
streamColumnParserArg ::
  forall b n m r.
  (BackendSchema b, MonadSchema n m, Has MkTypename r, MonadReader r m, MonadError QErr m, Has NamingCase r) =>
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
      fmap IR.openValueOrigin <$> columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)

-- | Input object parser whose keys are the column names and the values are the
--   initial values of those columns from where the streaming should start.
-- > input table_stream_cursor_value_input {
-- >   col1: col1_type
-- >   col2: col2_type
--     ...
-- > }
streamColumnValueParser ::
  forall b n m r.
  (BackendSchema b, MonadSchema n m, Has MkTypename r, MonadReader r m, MonadError QErr m, Has NamingCase r) =>
  SourceInfo b ->
  G.Name ->
  [ColumnInfo b] ->
  m (Parser 'Input n [(ColumnInfo b, ColumnValue b)])
streamColumnValueParser sourceInfo tableGQLName colInfos =
  memoizeOn 'streamColumnValueParser (_siName sourceInfo, tableGQLName) $ do
    tCase <- asks getter
    columnVals <- sequenceA <$> traverse streamColumnParserArg colInfos
    objName <- mkTypename $ tableGQLName <> applyTypeNameCaseCust tCase Name.__stream_cursor_value_input
    pure do
      let description = G.Description $ "Initial value of the column from where the streaming should start"
      P.object objName (Just description) columnVals <&> catMaybes

-- | Argument to accept the initial value from where the streaming should start.
-- > initial_value: table_stream_cursor_value_input!
streamColumnValueParserArg ::
  forall b n m r.
  ( BackendSchema b,
    MonadSchema n m,
    Has MkTypename r,
    MonadReader r m,
    MonadError QErr m,
    Has NamingCase r
  ) =>
  SourceInfo b ->
  G.Name ->
  [ColumnInfo b] ->
  m (InputFieldsParser n [(ColumnInfo b, ColumnValue b)])
streamColumnValueParserArg sourceInfo tableGQLName colInfos = do
  tCase <- asks getter
  columnValueParser <- streamColumnValueParser sourceInfo tableGQLName colInfos
  pure do
    P.field (applyFieldNameCaseCust tCase Name._initial_value) (Just $ G.Description "Stream column input with initial value") columnValueParser

-- | Argument to accept the cursor data. At the time of writing this, only a single
--   column cursor is supported and if multiple column cursors are provided,
--   then a parse error is thrown.
-- >
tableStreamColumnArg ::
  forall n m r b.
  (BackendSchema b, MonadSchema n m, Has MkTypename r, MonadReader r m, MonadError QErr m, Has NamingCase r) =>
  SourceInfo b ->
  G.Name ->
  [ColumnInfo b] ->
  m (InputFieldsParser n [IR.StreamCursorItem b])
tableStreamColumnArg sourceInfo tableGQLName colInfos = do
  cursorOrderingParser <- cursorOrderingArg
  streamColumnParser <- streamColumnValueParserArg sourceInfo tableGQLName colInfos
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
  SourceInfo b ->
  TableInfo b ->
  m (Parser 'Input n [(IR.StreamCursorItem b)])
tableStreamCursorExp sourceInfo tableInfo =
  memoizeOn 'tableStreamCursorExp (_siName sourceInfo, tableInfoName tableInfo) $ do
    tCase <- asks getter
    tableGQLName <- getTableGQLName tableInfo
    columnInfos <- tableSelectColumns sourceInfo tableInfo
    objName <- mkTypename $ tableGQLName <> applyTypeNameCaseCust tCase Name.__stream_cursor_input
    let description =
          G.Description $ "Streaming cursor of the table " <>> tableGQLName
    columnParsers <- tableStreamColumnArg sourceInfo tableGQLName columnInfos
    pure $ P.object objName (Just description) columnParsers

-- | Argument to accept the cursor input object.
-- > cursor: [table_stream_cursor_input]!
tableStreamCursorArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableInfo b ->
  m (InputFieldsParser n [IR.StreamCursorItem b])
tableStreamCursorArg sourceInfo tableInfo = do
  cursorParser <- tableStreamCursorExp sourceInfo tableInfo
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
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableInfo b ->
  m (InputFieldsParser n (SelectStreamArgs b))
tableStreamArguments sourceInfo tableInfo = do
  tCase <- asks getter
  whereParser <- tableWhereArg sourceInfo tableInfo
  cursorParser <- tableStreamCursorArg sourceInfo tableInfo
  pure $ do
    whereArg <- whereParser
    cursorArg <-
      cursorParser `P.bindFields` \case
        [] -> parseError "one streaming column field is expected"
        [c] -> pure c
        _ -> parseError "multiple column cursors are not supported yet"
    batchSizeArg <- cursorBatchSizeArg tCase
    pure $
      IR.SelectStreamArgsG whereArg batchSizeArg cursorArg

-- | Field parser for a streaming subscription for a table.
selectStreamTable ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b
  ) =>
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  m (Maybe (P.FieldParser n (StreamSelectExp b)))
selectStreamTable sourceInfo tableInfo fieldName description = runMaybeT $ do
  selectPermissions <- MaybeT $ tableSelectPermissions tableInfo
  xStreamSubscription <- hoistMaybe $ streamSubscriptionExtension @b
  stringifyNum <- retrieve soStringifyNum
  tableStreamArgsParser <- lift $ tableStreamArguments sourceInfo tableInfo
  selectionSetParser <- MaybeT $ tableSelectionList sourceInfo tableInfo
  lift $
    memoizeOn 'selectStreamTable (_siName sourceInfo, tableName, fieldName) $ do
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
