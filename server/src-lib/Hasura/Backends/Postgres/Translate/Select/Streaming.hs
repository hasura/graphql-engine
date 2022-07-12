-- | This module defines the top-level translation functions pertaining to
-- streaming selects into Postgres AST.
--
-- Streaming subscriptions are subscriptions based on a user-provided cursor
-- column. Unlike live queries, streaming subscriptions can be used to only get
-- the part that has changed in the query's response, although this will be
-- dependent on the user's choice of the cursor column. The streaming starts
-- from the initial value provided by the user.
module Hasura.Backends.Postgres.Translate.Select.Streaming
  ( mkStreamSQLSelect,
    selectStreamQuerySQL,
  )
where

import Control.Monad.Writer.Strict (runWriter)
import Database.PG.Query (Query, fromBuilder)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.IdentifierUniqueness (prefixNumToAliases)
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value (withConstructorFn)
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases (mkBaseTableColumnAlias)
import Hasura.Backends.Postgres.Translate.Select.Internal.Extractor (asJsonAggExtr)
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (generateSQLSelectFromArrayNode)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processAnnSimpleSelect)
import Hasura.Backends.Postgres.Translate.Types
  ( MultiRowSelectNode (MultiRowSelectNode),
    PermissionLimitSubQuery (PLSQNotRequired),
    SelectNode (SelectNode),
    SourcePrefixes (SourcePrefixes),
    orderByForJsonAgg,
  )
import Hasura.Backends.Postgres.Types.Column (unsafePGColumnToBackend)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
  ( AnnBoolExpFld (AVColumn),
    GBoolExp (BoolField),
    OpExpG (AGT, ALT),
    andAnnBoolExps,
  )
import Hasura.RQL.IR.OrderBy (OrderByItemG (OrderByItemG))
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Column
  ( ColumnInfo (ciColumn),
    ColumnValue (cvType),
  )
import Hasura.RQL.Types.Common
  ( FieldName (FieldName),
    JsonAggSelect (JASMultipleRows),
  )
import Hasura.RQL.Types.Subscription
  ( CursorOrdering (CODescending),
  )
import Hasura.SQL.Backend (BackendType (Postgres))
import Hasura.SQL.Types
  ( CollectableType (CollectableTypeArray, CollectableTypeScalar),
    ToSQL (toSQL),
  )

selectStreamQuerySQL ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind) =>
  AnnSimpleStreamSelect ('Postgres pgKind) ->
  Query
selectStreamQuerySQL sel =
  fromBuilder $ toSQL $ mkStreamSQLSelect sel

mkStreamSQLSelect ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  AnnSimpleStreamSelect ('Postgres pgKind) ->
  S.Select
mkStreamSQLSelect (AnnSelectStreamG () fields from perm args strfyNum) =
  let cursorArg = _ssaCursorArg args
      cursorColInfo = _sciColInfo cursorArg
      annOrderbyCol = AOCColumn cursorColInfo
      basicOrderType =
        bool S.OTAsc S.OTDesc $ _sciOrdering cursorArg == CODescending
      orderByItems =
        nonEmpty $ pure $ OrderByItemG (Just basicOrderType) annOrderbyCol Nothing
      cursorBoolExp =
        let orderByOpExp = bool ALT AGT $ basicOrderType == S.OTAsc
            sqlExp =
              fromResVars
                (CollectableTypeScalar $ unsafePGColumnToBackend $ cvType (_sciInitialValue cursorArg))
                ["cursor", getPGColTxt $ ciColumn cursorColInfo]
         in BoolField $ AVColumn cursorColInfo [(orderByOpExp sqlExp)]

      selectArgs =
        noSelectArgs
          { _saWhere =
              Just $ maybe cursorBoolExp (andAnnBoolExps cursorBoolExp) $ _ssaWhere args,
            _saOrderBy = orderByItems,
            _saLimit = Just $ _ssaBatchSize args
          }
      sqlSelect = AnnSelectG fields from perm selectArgs strfyNum
      permLimitSubQuery = PLSQNotRequired
      ((selectSource, nodeExtractors), joinTree) =
        runWriter $
          flip runReaderT strfyNum $
            processAnnSimpleSelect sourcePrefixes rootFldName permLimitSubQuery sqlSelect
      selectNode = SelectNode nodeExtractors joinTree
      topExtractor =
        asJsonAggExtr JASMultipleRows rootFldAls permLimitSubQuery $
          orderByForJsonAgg selectSource
      cursorLatestValueExp :: S.SQLExp =
        let pgColumn = ciColumn cursorColInfo
            mkMaxOrMinSQLExp maxOrMin col =
              S.SEFnApp maxOrMin [S.SEIdentifier col] Nothing
            maxOrMinTxt = bool "MIN" "MAX" $ basicOrderType == S.OTAsc
            -- text encoding the cursor value while it's fetched from the DB, because
            -- we can then directly reuse this value, otherwise if this were json encoded
            -- then we'd have to parse the value and then convert it into a text encoded value
            colExp =
              [ S.SELit (getPGColTxt pgColumn),
                S.SETyAnn
                  (mkMaxOrMinSQLExp maxOrMinTxt $ mkBaseTableColumnAlias rootFldIdentifier pgColumn)
                  S.textTypeAnn
              ]
         in -- SELECT json_build_object ('col1', MAX(col1) :: text)

            S.SEFnApp "json_build_object" colExp Nothing
      cursorLatestValueExtractor = S.Extractor cursorLatestValueExp (Just $ S.Alias $ Identifier "cursor")
      arrayNode = MultiRowSelectNode [topExtractor, cursorLatestValueExtractor] selectNode
   in prefixNumToAliases $
        generateSQLSelectFromArrayNode selectSource arrayNode $ S.BELit True
  where
    rootFldIdentifier = toIdentifier rootFldName
    sourcePrefixes = SourcePrefixes rootFldIdentifier rootFldIdentifier
    rootFldName = FieldName "root"
    rootFldAls = S.Alias $ toIdentifier rootFldName

    -- TODO: these functions also exist in `resolveMultiplexedValue`, de-duplicate these!
    fromResVars pgType jPath =
      addTypeAnnotation pgType $
        S.SEOpApp
          (S.SQLOp "#>>")
          [ S.SEQIdentifier $ S.QIdentifier (S.QualifiedIdentifier (Identifier "_subs") Nothing) (Identifier "result_vars"),
            S.SEArray $ map S.SELit jPath
          ]
    addTypeAnnotation pgType =
      flip S.SETyAnn (S.mkTypeAnn pgType) . case pgType of
        CollectableTypeScalar scalarType -> withConstructorFn scalarType
        CollectableTypeArray _ -> id
