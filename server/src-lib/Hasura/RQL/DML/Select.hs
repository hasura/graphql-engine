module Hasura.RQL.DML.Select
  ( selectP2
  , convSelectQuery
  , runSelect
  )
where

import           Hasura.Prelude

import qualified Data.HashSet                              as HS
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Sequence                             as DS
import qualified Database.PG.Query                         as Q

import           Data.Aeson.Types
import           Data.Text.Extended
import           Instances.TH.Lift                         ()
import           Language.Haskell.TH.Syntax                (Lift)

import qualified Hasura.Backends.Postgres.SQL.DML          as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.Select
import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types


type SelectQExt b = SelectG (ExtCol b) (BoolExp b) Int

-- Columns in RQL
-- This technically doesn't need to be generalized to all backends as
-- it is specific to this module; however the generalization work was
-- already done, and there's no particular reason to force this to be
-- specific.
data ExtCol (b :: Backend)
  = ECSimple !(Column b)
  | ECRel !RelName !(Maybe RelName) !(SelectQExt b)
deriving instance Lift (ExtCol 'Postgres)

instance ToJSON (ExtCol 'Postgres) where
  toJSON (ECSimple s) = toJSON s
  toJSON (ECRel rn mrn selq) =
    object $ [ "name" .= rn
             , "alias" .= mrn
             ] ++ selectGToPairs selq

instance FromJSON (ExtCol 'Postgres) where
  parseJSON v@(Object o) =
    ECRel
    <$> o .:  "name"
    <*> o .:? "alias"
    <*> parseJSON v
  parseJSON v@(String _) =
    ECSimple <$> parseJSON v
  parseJSON _ =
    fail $ mconcat
    [ "A column should either be a string or an "
    , "object (relationship)"
    ]

convSelCol :: (UserInfoM m, QErrM m, CacheRM m)
           => FieldInfoMap (FieldInfo 'Postgres)
           -> SelPermInfo 'Postgres
           -> SelCol 'Postgres
           -> m [ExtCol 'Postgres]
convSelCol _ _ (SCExtSimple cn) =
  return [ECSimple cn]
convSelCol fieldInfoMap _ (SCExtRel rn malias selQ) = do
  -- Point to the name key
  let pgWhenRelErr = "only relationships can be expanded"
  relInfo <- withPathK "name" $
    askRelType fieldInfoMap rn pgWhenRelErr
  let (RelInfo _ _ _ relTab _ _) = relInfo
  (rfim, rspi) <- fetchRelDet rn relTab
  resolvedSelQ <- resolveStar rfim rspi selQ
  return [ECRel rn malias resolvedSelQ]
convSelCol fieldInfoMap spi (SCStar wildcard) =
  convWildcard fieldInfoMap spi wildcard

convWildcard
  :: (UserInfoM m, QErrM m, CacheRM m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> SelPermInfo 'Postgres
  -> Wildcard
  -> m [ExtCol 'Postgres]
convWildcard fieldInfoMap selPermInfo wildcard =
  case wildcard of
  Star         -> return simpleCols
  (StarDot wc) -> (simpleCols ++) <$> (catMaybes <$> relExtCols wc)
  where
    cols = spiCols selPermInfo
    pgCols = map pgiColumn $ getCols fieldInfoMap
    relColInfos = getRels fieldInfoMap

    simpleCols = map ECSimple $ filter (`HS.member` cols) pgCols

    mkRelCol wc relInfo = do
      let relName = riName relInfo
          relTab  = riRTable relInfo
      relTabInfo <- fetchRelTabInfo relTab
      mRelSelPerm <- askPermInfo' PASelect relTabInfo

      forM mRelSelPerm $ \rspi -> do
        rExtCols <- convWildcard (_tciFieldInfoMap $ _tiCoreInfo relTabInfo) rspi wc
        return $ ECRel relName Nothing $
          SelectG rExtCols Nothing Nothing Nothing Nothing

    relExtCols wc = mapM (mkRelCol wc) relColInfos

resolveStar :: (UserInfoM m, QErrM m, CacheRM m)
            => FieldInfoMap (FieldInfo 'Postgres)
            -> SelPermInfo 'Postgres
            -> SelectQ 'Postgres
            -> m (SelectQExt 'Postgres)
resolveStar fim spi (SelectG selCols mWh mOb mLt mOf) = do
  procOverrides <- fmap (concat . catMaybes) $ withPathK "columns" $
    indexedForM selCols $ \selCol -> case selCol of
    (SCStar _) -> return Nothing
    _          -> Just <$> convSelCol fim spi selCol
  everything <- case wildcards of
    [] -> return []
    _  -> convWildcard fim spi $ maximum wildcards
  let extCols = unionBy equals procOverrides everything
  return $ SelectG extCols mWh mOb mLt mOf
  where
    wildcards = lefts $ map mkEither selCols

    mkEither (SCStar wc) = Left wc
    mkEither selCol      = Right selCol

    equals (ECSimple x) (ECSimple y)   = x == y
    equals (ECRel x _ _) (ECRel y _ _) = x == y
    equals _ _                         = False

convOrderByElem
  :: (UserInfoM m, QErrM m, CacheRM m)
  => SessVarBldr 'Postgres m
  -> (FieldInfoMap (FieldInfo 'Postgres), SelPermInfo 'Postgres)
  -> OrderByCol
  -> m (AnnOrderByElement 'Postgres S.SQLExp)
convOrderByElem sessVarBldr (flds, spi) = \case
  OCPG fldName -> do
    fldInfo <- askFieldInfo flds fldName
    case fldInfo of
      FIColumn colInfo -> do
        checkSelOnCol spi (pgiColumn colInfo)
        let ty = pgiType colInfo
        if isScalarColumnWhere isGeoType ty
          then throw400 UnexpectedPayload $ mconcat
           [ fldName <<> " has type 'geometry'"
           , " and cannot be used in order_by"
           ]
          else return $ AOCColumn colInfo
      FIRelationship _ -> throw400 UnexpectedPayload $ mconcat
        [ fldName <<> " is a"
        , " relationship and should be expanded"
        ]
      FIComputedField _ -> throw400 UnexpectedPayload $ mconcat
        [ fldName <<> " is a"
        , " computed field and can't be used in 'order_by'"
        ]
      -- TODO Rakesh (from master)
      FIRemoteRelationship {} ->
        throw400 UnexpectedPayload (mconcat [ fldName <<> " is a remote field" ])
  OCRel fldName rest -> do
    fldInfo <- askFieldInfo flds fldName
    case fldInfo of
      FIColumn _ -> throw400 UnexpectedPayload $ mconcat
        [ fldName <<> " is a Postgres column"
        , " and cannot be chained further"
        ]
      FIComputedField _ -> throw400 UnexpectedPayload $ mconcat
        [ fldName <<> " is a"
        , " computed field and can't be used in 'order_by'"
        ]
      FIRelationship relInfo -> do
        when (riType relInfo == ArrRel) $
          throw400 UnexpectedPayload $ mconcat
          [ fldName <<> " is an array relationship"
          ," and can't be used in 'order_by'"
          ]
        (relFim, relSpi) <- fetchRelDet (riName relInfo) (riRTable relInfo)
        resolvedSelFltr <- convAnnBoolExpPartialSQL sessVarBldr $ spiFilter relSpi
        AOCObjectRelation relInfo resolvedSelFltr <$>
          convOrderByElem sessVarBldr (relFim, relSpi) rest
      FIRemoteRelationship {} ->
        throw400 UnexpectedPayload (mconcat [ fldName <<> " is a remote field" ])

convSelectQ
  :: (UserInfoM m, QErrM m, CacheRM m, HasSQLGenCtx m)
  => QualifiedTable
  -> FieldInfoMap (FieldInfo 'Postgres)  -- Table information of current table
  -> SelPermInfo 'Postgres   -- Additional select permission info
  -> SelectQExt 'Postgres     -- Given Select Query
  -> SessVarBldr 'Postgres m
  -> (PGColumnType -> Value -> m S.SQLExp)
  -> m (AnnSimpleSel 'Postgres)
convSelectQ table fieldInfoMap selPermInfo selQ sessVarBldr prepValBldr = do

  annFlds <- withPathK "columns" $
    indexedForM (sqColumns selQ) $ \case
    (ECSimple pgCol) -> do
      colInfo <- convExtSimple fieldInfoMap selPermInfo pgCol
      return (fromPGCol pgCol, mkAnnColumnField colInfo Nothing)
    (ECRel relName mAlias relSelQ) -> do
      annRel <- convExtRel fieldInfoMap relName mAlias
                relSelQ sessVarBldr prepValBldr
      return ( fromRel $ fromMaybe relName mAlias
             , either AFObjectRelation AFArrayRelation annRel
             )

  -- Convert where clause
  wClause <- forM (sqWhere selQ) $ \be ->
    withPathK "where" $
    convBoolExp fieldInfoMap selPermInfo be sessVarBldr prepValBldr

  annOrdByML <- forM (sqOrderBy selQ) $ \(OrderByExp obItems) ->
    withPathK "order_by" $ indexedForM obItems $ mapM $
    convOrderByElem sessVarBldr (fieldInfoMap, selPermInfo)

  let annOrdByM = NE.nonEmpty =<< annOrdByML

  -- validate limit and offset values
  withPathK "limit" $ mapM_ onlyPositiveInt mQueryLimit
  withPathK "offset" $ mapM_ onlyPositiveInt mQueryOffset

  resolvedSelFltr <- convAnnBoolExpPartialSQL sessVarBldr $
                     spiFilter selPermInfo

  let tabFrom = FromTable table
      tabPerm = TablePerm resolvedSelFltr mPermLimit
      tabArgs = SelectArgs wClause annOrdByM mQueryLimit
                (S.intToSQLExp <$> mQueryOffset) Nothing

  strfyNum <- stringifyNum <$> askSQLGenCtx
  return $ AnnSelectG annFlds tabFrom tabPerm tabArgs strfyNum

  where
    mQueryOffset = sqOffset selQ
    mQueryLimit = sqLimit selQ
    mPermLimit = spiLimit selPermInfo

convExtSimple
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> SelPermInfo 'Postgres
  -> PGCol
  -> m (ColumnInfo 'Postgres)
convExtSimple fieldInfoMap selPermInfo pgCol = do
  checkSelOnCol selPermInfo pgCol
  askPGColInfo fieldInfoMap pgCol relWhenPGErr
  where
    relWhenPGErr = "relationships have to be expanded"

convExtRel
  :: (UserInfoM m, QErrM m, CacheRM m, HasSQLGenCtx m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> RelName
  -> Maybe RelName
  -> SelectQExt 'Postgres
  -> SessVarBldr 'Postgres m
  -> (PGColumnType -> Value -> m S.SQLExp)
  -> m (Either (ObjectRelationSelect 'Postgres) (ArraySelect 'Postgres))
convExtRel fieldInfoMap relName mAlias selQ sessVarBldr prepValBldr = do
  -- Point to the name key
  relInfo <- withPathK "name" $
    askRelType fieldInfoMap relName pgWhenRelErr
  let (RelInfo _ relTy colMapping relTab _ _) = relInfo
  (relCIM, relSPI) <- fetchRelDet relName relTab
  annSel <- convSelectQ relTab relCIM relSPI selQ sessVarBldr prepValBldr
  case relTy of
    ObjRel -> do
      when misused $ throw400 UnexpectedPayload objRelMisuseMsg
      return $ Left $ AnnRelationSelectG (fromMaybe relName mAlias) colMapping $
        AnnObjectSelectG (_asnFields annSel) relTab $ _tpFilter $ _asnPerm annSel
    ArrRel ->
      return $ Right $ ASSimple $ AnnRelationSelectG (fromMaybe relName mAlias)
               colMapping annSel
  where
    pgWhenRelErr = "only relationships can be expanded"
    misused      =
      or [ isJust (sqWhere selQ)
         , isJust (sqLimit selQ)
         , isJust (sqOffset selQ)
         , isJust (sqOrderBy selQ)
         ]
    objRelMisuseMsg =
      mconcat [ "when selecting an 'obj_relationship' "
              , "'where', 'order_by', 'limit' and 'offset' "
              , " can't be used"
              ]

convSelectQuery
  :: (UserInfoM m, QErrM m, CacheRM m, HasSQLGenCtx m)
  => SessVarBldr 'Postgres m
  -> (ColumnType 'Postgres -> Value -> m S.SQLExp)
  -> SelectQuery
  -> m (AnnSimpleSel  'Postgres)
convSelectQuery sessVarBldr prepArgBuilder (DMLQuery qt selQ) = do
  tabInfo     <- withPathK "table" $ askTabInfo qt
  selPermInfo <- askSelPermInfo tabInfo
  let fieldInfo = _tciFieldInfoMap $ _tiCoreInfo tabInfo
  extSelQ <- resolveStar fieldInfo selPermInfo selQ
  validateHeaders $ spiRequiredHeaders selPermInfo
  convSelectQ qt fieldInfo selPermInfo extSelQ sessVarBldr prepArgBuilder

selectP2 :: JsonAggSelect -> (AnnSimpleSel 'Postgres, DS.Seq Q.PrepArg) -> Q.TxE QErr EncJSON
selectP2 jsonAggSelect (sel, p) =
  encJFromBS . runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder selectSQL) (toList p) True
  where
    selectSQL = toSQL $ mkSQLSelect jsonAggSelect sel

phaseOne
  :: (QErrM m, UserInfoM m, CacheRM m, HasSQLGenCtx m)
  => SelectQuery -> m (AnnSimpleSel  'Postgres, DS.Seq Q.PrepArg)
phaseOne =
  runDMLP1T . convSelectQuery sessVarFromCurrentSetting binRHSBuilder

phaseTwo :: (MonadTx m) => (AnnSimpleSel 'Postgres, DS.Seq Q.PrepArg) -> m EncJSON
phaseTwo =
  liftTx . selectP2 JASMultipleRows

runSelect
  :: (QErrM m, UserInfoM m, CacheRM m, HasSQLGenCtx m, MonadTx m)
  => SelectQuery -> m EncJSON
runSelect q =
  phaseOne q >>= phaseTwo
