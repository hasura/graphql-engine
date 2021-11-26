module Hasura.GraphQL.Schema.SubscriptionStream
  (  )
where

import Data.Has
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended
import Hasura.GraphQL.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Schema (Typename (..))
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.OrderBy qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G
import Hasura.GraphQL.Parser.Internal.Input (InputFieldsParser(InputFieldsParser))
import Hasura.Base.Error (QErr)

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
  m (InputFieldsParser n (UnpreparedValue b))
streamColumnParserArg colInfo = do
  fieldParser <- typedParser colInfo
  let fieldName = pgiName colInfo
      fieldDesc = pgiDescription colInfo
  pure do
    P.field fieldName fieldDesc fieldParser
  where
    typedParser columnInfo = do
      colParser <- columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure (fmap (P.mkCursorParameter (pgiName columnInfo)) colParser)


tableStreamColumnArg ::
  forall n m r b.
  (MonadBuildSchema b r m n, Monad (InputFieldsParser n)) =>
  ColumnInfo b ->
  m (InputFieldsParser n (IR.StreamColumnItem b))
tableStreamColumnArg colInfo = do
  cursorOrderingParser <- cursorOrderingArg
  streamColumnParser <- streamColumnParserArg colInfo
  pure do
    columnArg <- streamColumnParser
    orderingArg <- cursorOrderingParser
    pure $ IR.StreamColumnItem columnArg (fromMaybe COAscending orderingArg)

-- streamingCursorExp ::
--   forall m n r b.
--   (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has P.MkTypename r) =>
--   SourceName ->
--   TableInfo b ->
--   SelPermInfo b ->
--   m (InputFieldsParser n [(ColumnInfo b, (UnpreparedValue b, CursorOrdering))])
-- streamingCursorExp sourceName tableInfo selectPermissions = memoizeOn 'streamingCursorExp (sourceName, tableInfoName tableInfo) $ do
--   tableGQLName <- getTableGQLName tableInfo
--   name <- P.mkTypename $ tableGQLName <> $$(G.litName "_stream_cursor_input")
--   let description =
--         G.Description $
--           "Cursor input options when streaming data from " <> tableInfoName tableInfo <<> "."
--   columnInfos <- tableSelectColumns sourceName tableInfo selectPermissions
--   fields <-
--     for columnInfos $ \columnInfo -> do
--       let fieldName = pgiName columnInfo
--           fieldDesc = pgiDescription columnInfo
--       objName <- P.mkTypename $ tableGQLName <> $$(G.litName "_") <> fieldName <> $$(G.litName "_streaming_column")
--       fieldParser <- typedParser columnInfo
--       let fieldInputField = P.field fieldName fieldDesc fieldParser
--           orderingInputField = P.
--       pure $
--         P.fieldOptional fieldName (Just fieldDesc) $ do
--           P.object objName (Just $ G.Description $ "streaming input object for the " <> tableGQLName <<> "'s column: " <<> fieldName)
--   pure []
--   where
--     typedParser columnInfo =
--       fmap (P.mkCursorParameter (pgiName columnInfo)) <$> columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)

--     mkStreamColParser ::
--       ColumnInfo b ->
--       m (InputFieldsParser n (UnpreparedValue b, CursorOrdering))
--     mkStreamColParser colInfo = do
--       orderingParser <- cursorOrderingArgParser
--       valueParser <- typedParser colInfo
--       pure do
--         orderingArg <- orderingParser
--         valueArg <- valueParser
--         pure (valueArg, orderingArg)
