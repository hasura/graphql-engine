module Hasura.RQL.DML.Select
  ( runSelect,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as DS
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.Translate.Select
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (selectToSelectWithM, toQuery)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.LogicalModel.Cache (LogicalModelCache, LogicalModelInfo (..))
import Hasura.LogicalModel.Fields (LogicalModelFieldsRM, runLogicalModelFieldsLookup)
import Hasura.Prelude
import Hasura.RQL.DML.Internal
import Hasura.RQL.DML.Types
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.Session
import Hasura.Table.Cache
import Hasura.Tracing qualified as Tracing

type SelectQExt = SelectG (ExtCol ('Postgres 'Vanilla)) (BoolExp ('Postgres 'Vanilla)) Int

-- Columns in RQL
-- This technically doesn't need to be generalized to all backends as
-- it is specific to this module; however the generalization work was
-- already done, and there's no particular reason to force this to be
-- specific.
data ExtCol (b :: BackendType)
  = ECSimple (Column b)
  | ECRel RelName (Maybe RelName) SelectQExt

convSelCol ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  SelPermInfo ('Postgres 'Vanilla) ->
  SelCol ->
  m [ExtCol ('Postgres 'Vanilla)]
convSelCol _ _ (SCExtSimple cn) =
  pure [ECSimple cn]
convSelCol fieldInfoMap _ (SCExtRel rn malias selQ) = do
  -- Point to the name key
  let pgWhenRelErr = "only relationships can be expanded"
  relInfo <-
    withPathK "name"
      $ askRelType fieldInfoMap rn pgWhenRelErr
  case relInfo of
    (RelInfo {riTarget = RelTargetNativeQuery _}) -> error "convSelCol RelTargetNativeQuery"
    (RelInfo {riTarget = RelTargetTable relTable}) -> do
      (rfim, rspi) <- fetchRelDet rn relTable
      resolvedSelQ <- resolveStar rfim rspi selQ
      pure [ECRel rn malias resolvedSelQ]
convSelCol fieldInfoMap spi (SCStar wildcard) =
  convWildcard fieldInfoMap spi wildcard

convWildcard ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  SelPermInfo ('Postgres 'Vanilla) ->
  Wildcard ->
  m [ExtCol ('Postgres 'Vanilla)]
convWildcard fieldInfoMap selPermInfo wildcard =
  case wildcard of
    Star -> pure simpleCols
    (StarDot wc) -> (simpleCols ++) <$> (catMaybes <$> relExtCols wc)
  where
    cols = spiCols selPermInfo
    pgCols = map structuredColumnInfoColumn $ getCols fieldInfoMap
    relColInfos = getRels fieldInfoMap

    simpleCols = map ECSimple $ filter (`HashMap.member` cols) pgCols

    mkRelCol _wc (RelInfo {riTarget = RelTargetNativeQuery _}) =
      error "convWildcard RelTargetNativeQuery"
    mkRelCol wc (RelInfo {riName = relName, riTarget = RelTargetTable relTableName}) = do
      relTabInfo <- fetchRelTabInfo relTableName
      mRelSelPerm <- askPermInfo permSel relTabInfo

      forM mRelSelPerm $ \relSelPermInfo -> do
        rExtCols <- convWildcard (_tciFieldInfoMap $ _tiCoreInfo relTabInfo) relSelPermInfo wc
        pure
          $ ECRel relName Nothing
          $ SelectG rExtCols Nothing Nothing Nothing Nothing

    relExtCols wc = mapM (mkRelCol wc) relColInfos

resolveStar ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  SelPermInfo ('Postgres 'Vanilla) ->
  SelectQ ->
  m SelectQExt
resolveStar fim selPermInfo (SelectG selCols mWh mOb mLt mOf) = do
  procOverrides <- fmap (concat . catMaybes)
    $ withPathK "columns"
    $ indexedForM selCols
    $ \selCol -> case selCol of
      (SCStar _) -> pure Nothing
      _ -> Just <$> convSelCol fim selPermInfo selCol
  everything <- case wildcards of
    [] -> pure []
    _ -> convWildcard fim selPermInfo $ maximum wildcards
  let extCols = unionBy equals procOverrides everything
  pure $ SelectG extCols mWh mOb mLt mOf
  where
    wildcards = lefts $ map mkEither selCols

    mkEither (SCStar wc) = Left wc
    mkEither selCol = Right selCol

    equals (ECSimple x) (ECSimple y) = x == y
    equals (ECRel x _ _) (ECRel y _ _) = x == y
    equals _ _ = False

convOrderByElem ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  SessionVariableBuilder m ->
  (FieldInfoMap (FieldInfo ('Postgres 'Vanilla)), SelPermInfo ('Postgres 'Vanilla)) ->
  OrderByCol ->
  m (AnnotatedOrderByElement ('Postgres 'Vanilla) S.SQLExp)
convOrderByElem sessVarBldr (flds, spi) = \case
  OCPG fldName -> do
    fldInfo <- askFieldInfo flds fldName
    case fldInfo of
      FIColumn (SCIScalarColumn colInfo) -> do
        checkSelOnCol spi (ciColumn colInfo)
        let redactionExp = fromMaybe NoRedaction $ HashMap.lookup (ciColumn colInfo) (spiCols spi)
        resolvedRedactionExp <- convAnnRedactionExpPartialSQL sessVarBldr redactionExp
        let ty = ciType colInfo
        if isScalarColumnWhere isGeoType ty
          then
            throw400 UnexpectedPayload
              $ fldName
              <<> " has type 'geometry' and cannot be used in order_by"
          else pure $ AOCColumn colInfo resolvedRedactionExp
      FIRelationship _ ->
        throw400 UnexpectedPayload
          $ fldName
          <<> " is a relationship and should be expanded"
      FIComputedField _ ->
        throw400 UnexpectedPayload
          $ fldName
          <<> " is a computed field and can't be used in 'order_by'"
      -- TODO Rakesh (from master)
      FIRemoteRelationship {} ->
        throw400 UnexpectedPayload (fldName <<> " is a remote field")
  OCRel fldName rest -> do
    fldInfo <- askFieldInfo flds fldName
    case fldInfo of
      FIColumn _ ->
        throw400 UnexpectedPayload
          $ fldName
          <<> " is a Postgres column and cannot be chained further"
      FIComputedField _ ->
        throw400 UnexpectedPayload
          $ fldName
          <<> " is a computed field and can't be used in 'order_by'"
      FIRelationship relInfo -> do
        relTableName <- case riTarget relInfo of
          RelTargetTable tn -> pure tn
          RelTargetNativeQuery _ -> error "convOrderByElem RelTargetNativeQuery"
        when (riType relInfo == ArrRel)
          $ throw400 UnexpectedPayload
          $ fldName
          <<> " is an array relationship and can't be used in 'order_by'"
        (relFim, relSelPermInfo) <- fetchRelDet (riName relInfo) relTableName
        resolvedSelFltr <- convAnnBoolExpPartialSQL sessVarBldr $ spiFilter relSelPermInfo
        AOCObjectRelation relInfo resolvedSelFltr <$> convOrderByElem sessVarBldr (relFim, relSelPermInfo) rest
      FIRemoteRelationship {} ->
        throw400 UnexpectedPayload (fldName <<> " is a remote field")

convSelectQ ::
  ( UserInfoM m,
    QErrM m,
    TableInfoRM ('Postgres 'Vanilla) m,
    LogicalModelFieldsRM ('Postgres 'Vanilla) m
  ) =>
  SQLGenCtx ->
  TableName ('Postgres 'Vanilla) ->
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) -> -- Table information of current table
  SelPermInfo ('Postgres 'Vanilla) -> -- Additional select permission info
  SelectQExt -> -- Given Select Query
  SessionVariableBuilder m ->
  ValueParser ('Postgres 'Vanilla) m S.SQLExp ->
  m (AnnSimpleSelect ('Postgres 'Vanilla))
convSelectQ sqlGen table fieldInfoMap selPermInfo selQ sessVarBldr prepValBldr = do
  -- Convert where clause
  wClause <- forM (sqWhere selQ) $ \boolExp ->
    withPathK "where"
      $ convBoolExp fieldInfoMap selPermInfo boolExp sessVarBldr fieldInfoMap prepValBldr

  annFlds <- withPathK "columns"
    $ indexedForM (sqColumns selQ)
    $ \case
      (ECSimple pgCol) -> do
        (colInfo, redactionExp) <- convExtSimple fieldInfoMap selPermInfo pgCol
        resolvedRedactionExp <- convAnnRedactionExpPartialSQL sessVarBldr redactionExp
        pure (fromCol @('Postgres 'Vanilla) pgCol, mkAnnColumnField (ciColumn colInfo) (ciType colInfo) resolvedRedactionExp Nothing)
      (ECRel relName mAlias relSelQ) -> do
        annRel <-
          convExtRel
            sqlGen
            fieldInfoMap
            relName
            mAlias
            relSelQ
            sessVarBldr
            prepValBldr
        pure
          ( fromRel $ fromMaybe relName mAlias,
            either AFObjectRelation AFArrayRelation annRel
          )

  annOrdByML <- forM (sqOrderBy selQ) $ \(OrderByExp obItems) ->
    withPathK "order_by"
      $ indexedForM obItems
      $ mapM
      $ convOrderByElem sessVarBldr (fieldInfoMap, selPermInfo)

  let annOrdByM = NE.nonEmpty =<< annOrdByML

  -- validate limit and offset values
  withPathK "limit" $ mapM_ onlyPositiveInt mQueryLimit
  withPathK "offset" $ mapM_ onlyPositiveInt mQueryOffset

  resolvedSelFltr <-
    convAnnBoolExpPartialSQL sessVarBldr
      $ spiFilter selPermInfo

  let tabFrom = FromTable table
      tabPerm = TablePerm resolvedSelFltr mPermLimit
      tabArgs = SelectArgs wClause annOrdByM mQueryLimit (fromIntegral <$> mQueryOffset) Nothing
      strfyNum = stringifyNum sqlGen
  pure $ AnnSelectG annFlds tabFrom tabPerm tabArgs strfyNum Nothing
  where
    mQueryOffset = sqOffset selQ
    mQueryLimit = sqLimit selQ
    mPermLimit = spiLimit selPermInfo

convExtSimple ::
  (UserInfoM m, QErrM m) =>
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  SelPermInfo ('Postgres 'Vanilla) ->
  PGCol ->
  m (ColumnInfo ('Postgres 'Vanilla), AnnRedactionExpPartialSQL ('Postgres 'Vanilla))
convExtSimple fieldInfoMap selPermInfo pgCol = do
  checkSelOnCol selPermInfo pgCol
  colInfo <- askColInfo fieldInfoMap pgCol relWhenPGErr
  pure (colInfo, fromMaybe NoRedaction $ HashMap.lookup pgCol (spiCols selPermInfo))
  where
    relWhenPGErr = "relationships have to be expanded"

convExtRel ::
  ( UserInfoM m,
    QErrM m,
    TableInfoRM ('Postgres 'Vanilla) m,
    LogicalModelFieldsRM ('Postgres 'Vanilla) m
  ) =>
  SQLGenCtx ->
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  RelName ->
  Maybe RelName ->
  SelectQExt ->
  SessionVariableBuilder m ->
  ValueParser ('Postgres 'Vanilla) m S.SQLExp ->
  m (Either (ObjectRelationSelect ('Postgres 'Vanilla)) (ArraySelect ('Postgres 'Vanilla)))
convExtRel sqlGen fieldInfoMap relName mAlias selQ sessVarBldr prepValBldr = do
  -- Point to the name key
  relInfo <-
    withPathK "name"
      $ askRelType fieldInfoMap relName pgWhenRelErr
  let (RelInfo {riType = relTy, riMapping = RelMapping colMapping, riTarget = relTarget}) = relInfo
  relTableName <- case relTarget of
    RelTargetNativeQuery _ -> error "convExtRel RelTargetNativeQuery"
    RelTargetTable tn -> pure tn
  (relCIM, relSPI) <- fetchRelDet relName relTableName
  annSel <- convSelectQ sqlGen relTableName relCIM relSPI selQ sessVarBldr prepValBldr
  case relTy of
    ObjRel -> do
      when misused $ throw400 UnexpectedPayload objRelMisuseMsg
      pure
        $ Left
        $ AnnRelationSelectG (fromMaybe relName mAlias) colMapping Nullable
        $ AnnObjectSelectG (_asnFields annSel) (FromTable relTableName)
        $ _tpFilter
        $ _asnPerm annSel
    ArrRel ->
      pure
        $ Right
        $ ASSimple
        $ AnnRelationSelectG
          (fromMaybe relName mAlias)
          colMapping
          Nullable
          annSel
  where
    pgWhenRelErr = "only relationships can be expanded"
    misused =
      or
        [ isJust (sqWhere selQ),
          isJust (sqLimit selQ),
          isJust (sqOffset selQ),
          isJust (sqOrderBy selQ)
        ]
    objRelMisuseMsg =
      "when selecting an 'obj_relationship' 'where', 'order_by', 'limit' and 'offset'  can't be used"

convSelectQuery ::
  ( UserInfoM m,
    QErrM m,
    TableInfoRM ('Postgres 'Vanilla) m,
    LogicalModelFieldsRM ('Postgres 'Vanilla) m
  ) =>
  SQLGenCtx ->
  SessionVariableBuilder m ->
  ValueParser ('Postgres 'Vanilla) m S.SQLExp ->
  SelectQuery ->
  m (AnnSimpleSelect ('Postgres 'Vanilla))
convSelectQuery sqlGen sessVarBldr prepArgBuilder (DMLQuery _ qt selQ) = do
  tabInfo <- withPathK "table" $ askTableInfoSource qt
  selPermInfo <- askSelPermInfo tabInfo
  let fieldInfo = _tciFieldInfoMap $ _tiCoreInfo tabInfo
  extSelQ <- resolveStar fieldInfo selPermInfo selQ
  validateHeaders $ spiRequiredHeaders selPermInfo
  convSelectQ sqlGen qt fieldInfo selPermInfo extSelQ sessVarBldr prepArgBuilder

selectP2 :: UserInfo -> JsonAggSelect -> (AnnSimpleSelect ('Postgres 'Vanilla), DS.Seq PG.PrepArg) -> PG.TxE QErr EncJSON
selectP2 userInfo jsonAggSelect (sel, p) = do
  selectWithQuery <- selectToSelectWithM $ mkSQLSelect userInfo jsonAggSelect sel
  let selectSQL =
        toQuery
          $ selectWithQuery
  runIdentity
    . PG.getRow
    <$> PG.rawQE dmlTxErrorHandler selectSQL (toList p) True

phaseOne ::
  (QErrM m, UserInfoM m, CacheRM m) =>
  SQLGenCtx ->
  SelectQuery ->
  m (AnnSimpleSelect ('Postgres 'Vanilla), DS.Seq PG.PrepArg)
phaseOne sqlGen query = do
  let sourceName = getSourceDMLQuery query
  tableCache :: TableCache ('Postgres 'Vanilla) <- fold <$> askTableCache sourceName
  logicalModelCache :: LogicalModelCache ('Postgres 'Vanilla) <- fold <$> askLogicalModelCache sourceName
  flip runTableCacheRT tableCache
    $ runLogicalModelFieldsLookup _lmiFields logicalModelCache
    $ runDMLP1T
    $ convSelectQuery sqlGen sessVarFromCurrentSetting (valueParserWithCollectableType binRHSBuilder) query

phaseTwo :: (MonadTx m) => UserInfo -> (AnnSimpleSelect ('Postgres 'Vanilla), DS.Seq PG.PrepArg) -> m EncJSON
phaseTwo userInfo =
  liftTx . selectP2 userInfo JASMultipleRows

runSelect ::
  ( QErrM m,
    UserInfoM m,
    CacheRM m,
    MonadIO m,
    MonadBaseControl IO m,
    Tracing.MonadTrace m,
    MetadataM m
  ) =>
  SQLGenCtx ->
  SelectQuery ->
  m EncJSON
runSelect sqlGen q = do
  sourceConfig <- askSourceConfig @('Postgres 'Vanilla) (getSourceDMLQuery q)
  userInfo <- askUserInfo
  phaseOne sqlGen q >>= runTxWithCtx (_pscExecCtx sourceConfig) (Tx PG.ReadOnly Nothing) LegacyRQLQuery . (phaseTwo userInfo)
