{-# LANGUAGE ApplicativeDo #-}
module Hasura.GraphQL.Schema.SubscriptionStream
  ( tableStreamCursorArg,
    cursorBatchSizeArg
  )
where

import Data.Text.Extended ((<>>))
import Data.Has
import Data.List.NonEmpty qualified as NE
import Hasura.GraphQL.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.Prelude
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G
import Hasura.Base.Error (QErr)
import Hasura.GraphQL.Schema.Table (getTableGQLName, tableSelectColumns)

cursorBatchSizeArg ::
  forall n.
  MonadParse n =>
  InputFieldsParser n Int
cursorBatchSizeArg =
  fromIntegral <$>
    P.field batchSizeName batchSizeDesc P.nonNegativeInt
  where
    batchSizeName = $$(G.litName "batch_size")
    batchSizeDesc = Just $ G.Description "maximum number of rows returned in a single batch"

cursorOrderingArgParser ::
  forall n m r.
  (MonadSchema n m, Has P.MkTypename r, MonadReader r m) =>
  m (Parser 'Both n CursorOrdering)
cursorOrderingArgParser = do
  enumName <- P.mkTypename $$(G.litName "cursor_ordering")
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
          | enumNameVal <- [($$(G.litName "ASC"), COAscending), ($$(G.litName "DESC"), CODescending)]
        ]
  where
    define (name, val) =
      let orderingTypeDesc = bool "descending" "ascending" $ val == COAscending
       in P.mkDefinition name (Just $ G.Description $ orderingTypeDesc <> " ordering of the cursor") P.EnumValueInfo

cursorOrderingArg ::
  forall n m r.
  (MonadSchema n m, Has P.MkTypename r, MonadReader r m) =>
  m (InputFieldsParser n (Maybe CursorOrdering))
cursorOrderingArg = do
  cursorOrderingParser' <- cursorOrderingArgParser
  pure do
    P.fieldOptional $$(G.litName "ordering") (Just $ G.Description "cursor ordering") cursorOrderingParser'

streamColumnParserArg ::
  forall b n m r.
  (BackendSchema b, MonadSchema n m, Has P.MkTypename r, MonadReader r m, MonadError QErr m) =>
  ColumnInfo b ->
  m (InputFieldsParser n (Maybe (UnpreparedValue b)))
streamColumnParserArg colInfo = do
  fieldParser <- typedParser colInfo
  let fieldName = pgiName colInfo
      fieldDesc = pgiDescription colInfo
  pure do
    P.fieldOptional fieldName fieldDesc fieldParser
  where
    typedParser columnInfo = do
      colParser <- columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure (fmap (P.mkCursorParameter (pgiName columnInfo)) colParser)


tableStreamColumnArg ::
  forall n m r b.
  (BackendSchema b, MonadSchema n m, Has P.MkTypename r, MonadReader r m, MonadError QErr m) =>
  ColumnInfo b ->
  m (InputFieldsParser n (Maybe (IR.StreamColumnItem b (UnpreparedValue b))))
tableStreamColumnArg colInfo = do
  cursorOrderingParser <- cursorOrderingArg
  streamColumnParser <- streamColumnParserArg colInfo
  pure $ do
    orderingArg <- cursorOrderingParser
    columnArg <- streamColumnParser
    pure $ IR.StreamColumnItem orderingArg colInfo <$> columnArg

tableStreamCursorExp ::
  forall m n r b.
  (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has P.MkTypename r) =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Parser 'Input n [(IR.StreamColumnItem b (UnpreparedValue b))])
tableStreamCursorExp sourceName tableInfo selectPermissions =
  memoizeOn 'tableStreamCursorExp (sourceName, tableInfoName tableInfo) $ do
  tableGQLName <- getTableGQLName tableInfo
  columnInfos <- tableSelectColumns sourceName tableInfo selectPermissions
  objName <- P.mkTypename $ tableGQLName <> $$(G.litName ("_stream_cursor_input"))
  let description =
        G.Description $ "Streaming cursor of the table " <>> tableGQLName
  columnParsers <- sequenceA <$> traverse tableStreamColumnArg columnInfos
  pure $ P.object objName (Just description) columnParsers <&> catMaybes

tableStreamCursorArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (InputFieldsParser n [IR.StreamColumnItem b (UnpreparedValue b)])
tableStreamCursorArg sourceName tableInfo selectPermissions = do
  cursorParser <- tableStreamCursorExp sourceName tableInfo selectPermissions
  pure $ do
    cursorArgs <-
      P.field cursorName cursorDesc $ P.list $ P.nullable cursorParser
    pure $ concat $ catMaybes cursorArgs
  where
    cursorName = $$(G.litName "cursor")
    cursorDesc = Just $ G.Description "cursor to stream the results returned by the query"
