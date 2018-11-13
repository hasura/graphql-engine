{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Hasura.RQL.GBoolExp
  ( toSQLBoolExp
  , getBoolExpDeps
  , annBoolExp
  , txtRHSBuilder
  , pgValParser
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML      as S

import           Data.Aeson

import qualified Data.HashMap.Strict as M
import qualified Data.Text.Extended  as T

parseOpExp
  :: (MonadError QErr m)
  => ValueParser m a
  -> FieldInfoMap
  -> PGColInfo
  -> (T.Text, Value) -> m (OpExpG a)
parseOpExp parser fim (PGColInfo cn colTy _) (opStr, val) = case opStr of
  "$eq"       -> parseEq
  "_eq"       -> parseEq

  "$ne"       -> parseNe
  "_ne"       -> parseNe
  "$neq"      -> parseNe
  "_neq"      -> parseNe

  "$in"       -> parseIn
  "_in"       -> parseIn

  "$nin"      -> parseNin
  "_nin"      -> parseNin

  "$gt"       -> parseGt
  "_gt"       -> parseGt

  "$lt"       -> parseLt
  "_lt"       -> parseLt

  "$gte"      -> parseGte
  "_gte"      -> parseGte

  "$lte"      -> parseLte
  "_lte"      -> parseLte

  "$like"     -> parseLike
  "_like"     -> parseLike

  "$nlike"    -> parseNlike
  "_nlike"    -> parseNlike

  "$ilike"    -> parseIlike
  "_ilike"    -> parseIlike

  "$nilike"   -> parseNilike
  "_nilike"   -> parseNilike

  "$similar"  -> parseSimilar
  "_similar"  -> parseSimilar
  "$nsimilar" -> parseNsimilar
  "_nsimilar" -> parseNsimilar

  "$is_null"  -> parseIsNull
  "_is_null"  -> parseIsNull

  "$ceq"      -> parseCeq
  "_ceq"      -> parseCeq

  "$cne"      -> parseCne
  "_cne"      -> parseCne
  "$cneq"     -> parseCne
  "_cneq"     -> parseCne

  "$cgt"      -> parseCgt
  "_cgt"      -> parseCgt

  "$clt"      -> parseClt
  "_clt"      -> parseClt

  "$cgte"     -> parseCgte
  "_cgte"     -> parseCgte

  "$clte"     -> parseClte
  "_clte"     -> parseClte

  x           -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
  where
    parseEq       = AEQ <$> parseOne -- equals
    parseNe       = ANE <$> parseOne -- <>
    parseIn       = AIN <$> parseMany -- in an array
    parseNin      = ANIN <$> parseMany -- not in an array
    parseGt       = AGT <$> parseOne -- >
    parseLt       = ALT <$> parseOne -- <
    parseGte      = AGTE <$> parseOne -- >=
    parseLte      = ALTE <$> parseOne -- <=
    parseLike     = ALIKE <$> parseOne -- LIKE
    parseNlike    = ANLIKE <$> parseOne -- NOT LIKE
    parseIlike    = AILIKE <$> parseOne -- ILIKE, case insensitive
    parseNilike   = ANILIKE <$> parseOne -- NOT ILIKE, case insensitive
    parseSimilar  = ASIMILAR <$> parseOne -- similar, regex
    parseNsimilar = ANSIMILAR <$> parseOne -- not similar, regex
    parseIsNull   = bool ANISNOTNULL ANISNULL -- is null
                    <$> decodeValue val
    parseCeq      = CEQ <$> decodeAndValidateRhsCol
    parseCne      = CNE <$> decodeAndValidateRhsCol
    parseCgt      = CGT <$> decodeAndValidateRhsCol
    parseClt      = CLT <$> decodeAndValidateRhsCol
    parseCgte     = CGTE <$> decodeAndValidateRhsCol
    parseClte     = CLTE <$> decodeAndValidateRhsCol

    decodeAndValidateRhsCol =
      decodeValue val >>= validateRhsCol

    validateRhsCol rhsCol = do
      let errMsg = "column operators can only compare postgres columns"
      rhsType <- askPGType fim rhsCol errMsg
      if colTy /= rhsType
        then throw400 UnexpectedPayload $
             "incompatible column types : " <> cn <<> ", " <>> rhsCol
        else return rhsCol

    parseOne = parser colTy val
      -- runAesonParser (parsePGValue ty) val
    parseMany = do
      vals <- runAesonParser parseJSON val
      indexedForM vals (parser colTy)

parseOpExps
  :: (MonadError QErr m)
  => ValueParser m a
  -> FieldInfoMap
  -> PGColInfo
  -> Value
  -> m [OpExpG a]
parseOpExps valParser cim colInfo = \case
  (Object o) -> mapM (parseOpExp valParser cim colInfo)(M.toList o)
  val        -> pure . AEQ <$> valParser (pgiType colInfo) val

-- parseAnnOpExpG
--   :: (MonadError QErr m)
--   => (PGColType -> Value -> m a)
--   -> RQLOp -> PGColType -> Value -> m (AnnValOpExpG a)
-- parseAnnOpExpG parser op ty val = case op of
--   REQ       -> AEQ <$> parseOne -- equals
--   RNE       -> ANE <$> parseOne -- <>
--   RIN       -> AIN <$> parseMany -- in an array
--   RNIN      -> ANIN <$> parseMany -- not in an array
--   RGT       -> AGT <$> parseOne -- >
--   RLT       -> ALT <$> parseOne -- <
--   RGTE      -> AGTE <$> parseOne -- >=
--   RLTE      -> ALTE <$> parseOne -- <=
--   RLIKE     -> ALIKE <$> parseOne -- LIKE
--   RNLIKE    -> ANLIKE <$> parseOne -- NOT LIKE
--   RILIKE    -> AILIKE <$> parseOne -- ILIKE, case insensitive
--   RNILIKE   -> ANILIKE <$> parseOne -- NOT ILIKE, case insensitive
--   RSIMILAR  -> ASIMILAR <$> parseOne -- similar, regex
--   RNSIMILAR -> ANSIMILAR <$> parseOne -- not similar, regex
--   RISNULL   -> bool ANISNOTNULL ANISNULL -- is null
--                <$> decodeValue val
  -- where
  --   parseOne = parser ty val
  --     -- runAesonParser (parsePGValue ty) val
  --   parseMany = do
  --     vals <- runAesonParser parseJSON val
  --     indexedForM vals (parser ty)

-- isRQLOp :: T.Text -> Bool
-- isRQLOp t = case runIdentity . runExceptT $ parseOpExp t of
--   Left _  -> False
--   Right r -> either (const True) (const False) r

type ValueParser m a = PGColType -> Value -> m a

-- parseOpExps
--   :: (MonadError QErr m)
--   => ValueParser m a
--   -> FieldInfoMap
--   -> PGColInfo
--   -> Value
--   -> m [OpExpG a]
-- parseOpExps valParser cim (PGColInfo cn colTy _) (Object o) =
--   forM (M.toList o) $ \(k, v) -> do
--   op <- parseOpExp k
--   case (op, v) of
--     (Left rqlOp, _) -> do
--       modifyErr (cn <<>) $ getOpTypeChecker rqlOp colTy
--       annValOp <- withPathK (T.pack $ show rqlOp) $
--                   parseAnnOpExpG valParser rqlOp colTy v
--       return $ OEVal annValOp
--     (Right colOp, String c) -> do
--       let rhsCol  = PGCol c
--           errMsg = "column operators can only compare postgres columns"
--       rhsType <- askPGType cim rhsCol errMsg
--       when (colTy /= rhsType) $
--         throw400 UnexpectedPayload $
--         "incompatible column types : " <> cn <<> ", " <>> rhsCol
--       return $ OECol colOp rhsCol
--     (Right _, _) -> throw400 UnexpectedPayload "expecting a string for column operator"
-- parseOpExps valParser _ (PGColInfo _ colTy _) val = do
--   annValOp <- parseAnnOpExpG valParser REQ colTy val
--   return [OEVal annValOp]

buildMsg :: PGColType -> [PGColType] -> QErr
buildMsg ty expTys =
  err400 UnexpectedPayload $ mconcat
  [ " is of type " <> T.pack (show ty)
  , "; this operator works "
  , "only on columns of type "
  , T.intercalate "/" $ map (T.dquote . T.pack . show) expTys
  ]

type OpTypeChecker m = PGColType -> m ()

textOnlyOp :: (MonadError QErr m) => OpTypeChecker m
textOnlyOp PGText    = return ()
textOnlyOp PGVarchar = return ()
textOnlyOp ty =
  throwError $ buildMsg ty [PGVarchar, PGText]

-- validOnAllTypes :: (MonadError QErr m) => OpTypeChecker m
-- validOnAllTypes _ = return ()

-- getOpTypeChecker :: (MonadError QErr m) => RQLOp -> OpTypeChecker m
-- getOpTypeChecker REQ       = validOnAllTypes
-- getOpTypeChecker RNE       = validOnAllTypes
-- getOpTypeChecker RIN       = validOnAllTypes
-- getOpTypeChecker RNIN      = validOnAllTypes
-- getOpTypeChecker RGT       = validOnAllTypes
-- getOpTypeChecker RLT       = validOnAllTypes
-- getOpTypeChecker RGTE      = validOnAllTypes
-- getOpTypeChecker RLTE      = validOnAllTypes
-- getOpTypeChecker RLIKE     = textOnlyOp
-- getOpTypeChecker RNLIKE    = textOnlyOp
-- getOpTypeChecker RILIKE    = textOnlyOp
-- getOpTypeChecker RNILIKE   = textOnlyOp
-- getOpTypeChecker RSIMILAR  = textOnlyOp
-- getOpTypeChecker RNSIMILAR = textOnlyOp
-- getOpTypeChecker RISNULL   = validOnAllTypes

-- This convoluted expression instead of col = val
-- to handle the case of col : null
equalsBoolExpBuilder :: S.SQLExp -> S.SQLExp -> S.BoolExp
equalsBoolExpBuilder qualColExp rhsExp =
  S.BEBin S.OrOp (S.BECompare S.SEQ qualColExp rhsExp)
    (S.BEBin S.AndOp
      (S.BENull qualColExp)
      (S.BENull rhsExp))

notEqualsBoolExpBuilder :: S.SQLExp -> S.SQLExp -> S.BoolExp
notEqualsBoolExpBuilder qualColExp rhsExp =
  S.BEBin S.OrOp (S.BECompare S.SNE qualColExp rhsExp)
    (S.BEBin S.AndOp
      (S.BENotNull qualColExp)
      (S.BENull rhsExp))

-- mapBoolExp :: (Monad m)
--            => (a -> m b)
--            -> GBoolExp a -> m (GBoolExp b)
-- mapBoolExp f (BoolAnd bes)    = BoolAnd <$> mapM (mapBoolExp f) bes
-- mapBoolExp f (BoolOr bes)     = BoolOr  <$> mapM (mapBoolExp f) bes
-- mapBoolExp f (BoolFld ce)     = BoolFld <$> f ce
-- mapBoolExp f (BoolNot notExp) = BoolNot <$> mapBoolExp f notExp

annBoolExp
  :: (QErrM m, CacheRM m)
  => ValueParser m a
  -> FieldInfoMap
  -> BoolExp
  -> m (AnnBoolExp a)
annBoolExp valParser fim (BoolExp boolExp) =
  traverse (annColExp valParser fim) boolExp
-- annBoolExp valParser cim = \case
--   (BoolAnd bes)    -> BoolAnd <$> mapM (annBoolExp valParser cim) bes
--   (BoolOr bes)     -> BoolOr  <$> mapM (annBoolExp valParser cim) bes
--   (BoolFld ce)     -> BoolFld <$> annColExp valParser cim ce
--   (BoolNot notExp) -> BoolNot <$> annBoolExp valParser cim notExp

annColExp
  :: (QErrM m, CacheRM m)
  => ValueParser m a
  -> FieldInfoMap
  -> ColExp
  -> m (AnnBoolExpFld a)
annColExp valueParser colInfoMap (ColExp fieldName colVal) = do
  colInfo <- askFieldInfo colInfoMap fieldName
  case colInfo of
    FIColumn (PGColInfo _ PGJSON _) ->
      throwError (err400 UnexpectedPayload "JSON column can not be part of where clause")
    FIColumn (PGColInfo _ PGJSONB _) ->
      throwError (err400 UnexpectedPayload "JSONB column can not be part of where clause")
    FIColumn pgi ->
      AVCol pgi <$> parseOpExps valueParser colInfoMap pgi colVal
    FIRelationship relInfo -> do
      relBoolExp      <- decodeValue colVal
      relFieldInfoMap <- askFieldInfoMap $ riRTable relInfo
      annRelBoolExp   <- annBoolExp valueParser relFieldInfoMap relBoolExp
      return $ AVRel relInfo annRelBoolExp

-- toSQLBoolExp
--   :: (Monad m)
--   => BoolExpBuilder m a -> S.Qual
--   -> GBoolExp (AnnValO a) -> m S.BoolExp
-- toSQLBoolExp vp tq e =
--   evalStateT (convBoolRhs' vp tq e) 0

-- convBoolRhs'
--   :: (Monad m)
--   => BoolExpBuilder m a -> S.Qual
--   -> GBoolExp (AnnValO a) -> StateT Word64 m S.BoolExp
-- convBoolRhs' vp tq =
--   foldBoolExp (convColRhs vp tq)

-- convColRhs
--   :: (Monad m)
--   => BoolExpBuilder m a
--   -> S.Qual -> AnnValO a -> StateT Word64 m S.BoolExp
-- convColRhs bExpBuilder tableQual annVal = case annVal of
--   AVCol (PGColInfo cn _ _) opExps -> do
--     let qualColExp = mkQCol tableQual cn
--     -- bExps <- forM opExps $ \case
--     --   OEVal annOpValExp -> lift $ bExpBuilder qualColExp annOpValExp
--     --   OECol op rCol -> do
--     --     let rhsColExp = mkQCol tableQual rCol
--     --     return $ mkColOpSQLExp op qualColExp rhsColExp
--     bExps <- forM opExps $ \opExp ->
--       lift $ bExpBuilder tableQual qualColExp annOpValExp
--     return $ foldr (S.BEBin S.AndOp) (S.BELit True) bExps

--   AVRel (RelInfo _ _ colMapping relTN _ _) nesAnn -> do
--     -- Convert the where clause on the relationship
--     curVarNum <- get
--     put $ curVarNum + 1
--     let newIden  = Iden $ "_be_" <> T.pack (show curVarNum) <> "_"
--                    <> snakeCaseTable relTN
--         newIdenQ = S.QualIden newIden
--     annRelBoolExp <- convBoolRhs' bExpBuilder (Just newIdenQ) nesAnn
--     let backCompExp = foldr (S.BEBin S.AndOp) (S.BELit True) $
--           flip map colMapping $ \(lCol, rCol) ->
--           S.BECompare S.SEQ
--           (S.SEQIden $ S.QIden (S.QualIden newIden) (toIden rCol))
--           (mkQCol tableQual lCol)
--         innerBoolExp = S.BEBin S.AndOp backCompExp annRelBoolExp
--     -- return $ ABERel rn (relTN, newIden) annRelBoolExp backCompExp
--     return $ S.mkExists (S.FISimple relTN $ Just $ S.Alias newIden) innerBoolExp
--   where
--     mkQCol colQM col = case colQM of
--       Just colQ -> S.SEQIden $ S.QIden colQ $ toIden col
--       Nothing   -> S.SEIden $ toIden col

toSQLBoolExp
  :: S.Qual -> AnnBoolExpSQL -> S.BoolExp
toSQLBoolExp tq e =
  evalState (convBoolRhs' tq e) 0

convBoolRhs'
  :: S.Qual -> AnnBoolExpSQL -> State Word64 S.BoolExp
convBoolRhs' tq =
  foldBoolExp (convColRhs tq)

convColRhs
  :: S.Qual -> AnnBoolExpFldSQL -> State Word64 S.BoolExp
convColRhs tableQual = \case
  AVCol (PGColInfo cn _ _) opExps -> do
    let bExps = map (mkColCompExp tableQual cn) opExps
    return $ foldr (S.BEBin S.AndOp) (S.BELit True) bExps

  AVRel (RelInfo _ _ colMapping relTN _) nesAnn -> do
    -- Convert the where clause on the relationship
    curVarNum <- get
    put $ curVarNum + 1
    let newIden  = Iden $ "_be_" <> T.pack (show curVarNum) <> "_"
                   <> snakeCaseTable relTN
        newIdenQ = S.QualIden newIden
    annRelBoolExp <- convBoolRhs' newIdenQ nesAnn
    let backCompExp = foldr (S.BEBin S.AndOp) (S.BELit True) $
          flip map colMapping $ \(lCol, rCol) ->
          S.BECompare S.SEQ
          (mkQCol (S.QualIden newIden) rCol)
          (mkQCol tableQual lCol)
        innerBoolExp = S.BEBin S.AndOp backCompExp annRelBoolExp
    -- return $ ABERel rn (relTN, newIden) annRelBoolExp backCompExp
    return $ S.mkExists (S.FISimple relTN $ Just $ S.Alias newIden) innerBoolExp
  where
    mkQCol q = S.SEQIden . S.QIden q . toIden

    -- mkQCol colQM col = case colQM of
    --   Just colQ -> S.SEQIden $ S.QIden colQ $ toIden col
    --   Nothing   -> S.SEIden $ toIden col

-- cBoolExp
--   :: GBoolExp AnnSQLBoolExp
--   -> S.BoolExp
-- cBoolExp be =
--   runIdentity $ flip foldBoolExp be $ \ace ->
--   return $ cColExp ace

-- cColExp
--   :: AnnSQLBoolExp
--   -> S.BoolExp
-- cColExp annVal = case annVal of
--   ABECol _ be -> be
--   ABERel _ (tn, tIden) nesAnn backCompExp -> do
--     -- Convert the where clause on the relationship
--     let annRelBoolExp = cBoolExp nesAnn
--         innerBoolExp = S.BEBin S.AndOp backCompExp annRelBoolExp
--     S.mkExists (S.FISimple tn $ Just $ S.Alias tIden) innerBoolExp

-- txtValParser
--   :: (MonadError QErr m)
--   => ValueParser m (AnnValOpExpG S.SQLExp)
-- txtValParser =
--   undefined

pgValParser
  :: (MonadError QErr m)
  => PGColType -> Value -> m PGColValue
pgValParser ty =
  runAesonParser (parsePGValue ty)

txtRHSBuilder
  :: (MonadError QErr m)
  => PGColType -> Value -> m S.SQLExp
txtRHSBuilder ty val =
  txtEncoder <$> pgValParser ty val

-- this does not parse the value
noValParser
  :: (MonadError QErr m)
  => ValueParser m Value
noValParser _ = return

-- mkColCompExp
--   :: S.Qual -> PGCol -> OpExpG S.SQLExp -> S.BoolExp
-- mkColCompExp qual lhsCol = \case
--   AEQ val          -> equalsBoolExpBuilder lhs val
--   ANE val          -> notEqualsBoolExpBuilder lhs val
--   AIN  vals        -> mkInOrNotBoolExpBuilder True vals
--   ANIN vals        -> mkInOrNotBoolExpBuilder False vals
--   AGT val          -> mkSimpleBoolExpBuilder (S.BECompare S.SGT) val
--   ALT val          -> mkSimpleBoolExpBuilder (S.BECompare S.SLT) val
--   AGTE val         -> mkSimpleBoolExpBuilder (S.BECompare S.SGTE) val
--   ALTE val         -> mkSimpleBoolExpBuilder (S.BECompare S.SLTE) val
--   ALIKE val        -> mkSimpleBoolExpBuilder (S.BECompare S.SLIKE) val
--   ANLIKE val       -> mkSimpleBoolExpBuilder (S.BECompare S.SNLIKE) val
--   AILIKE val       -> mkSimpleBoolExpBuilder (S.BECompare S.SILIKE) val
--   ANILIKE val      -> mkSimpleBoolExpBuilder (S.BECompare S.SNILIKE) val
--   ASIMILAR val     -> mkSimpleBoolExpBuilder (S.BECompare S.SSIMILAR) val
--   ANSIMILAR val    -> mkSimpleBoolExpBuilder (S.BECompare S.SNSIMILAR) val
--   AContains val    -> mkSimpleBoolExpBuilder (S.BECompare S.SContains) val
--   AContainedIn val -> mkSimpleBoolExpBuilder (S.BECompare S.SContainedIn) val
--   AHasKey val      -> mkSimpleBoolExpBuilder (S.BECompare S.SHasKey) val
--   AHasKeysAny keys -> return $ S.BECompare S.SHasKeysAny lhs $ toTextArray keys
--   AHasKeysAll keys -> return $ S.BECompare S.SHasKeysAll lhs $ toTextArray keys
--   ANISNULL         -> return $ S.BENull lhs
--   ANISNOTNULL      -> return $ S.BENotNull lhs
--   CEQ rhsCol       -> return $ S.BECompare S.SEQ lhs rhsCol
--   CNE rhsCol       -> return $ S.BECompare S.SNE lhs rhsCol
--   CGT rhsCol       -> return $ S.BECompare S.SGT lhs rhsCol
--   CLT rhsCol       -> return $ S.BECompare S.SLT lhs rhsCol
--   CGTE rhsCol      -> return $ S.BECompare S.SGTE lhs rhsCol
--   CLTE rhsCol      -> return $ S.BECompare S.SLTE lhs rhsCol
--   where
--     lhs = undefined

--     toTextArray arr =
--       S.SETyAnn (S.SEArray $ map (txtEncoder . PGValText) arr) S.textArrType

--     mkSimpleBoolExpBuilder beF pgColVal =
--       beF lhs <$> rhsBldr pgColVal

--     mkInOrNotBoolExpBuilder isIn arrVals = do
--       rhsExps <- mapM rhsBldr arrVals
--       let boolExp = inBoolExpBuilder lhs rhsExps
--       return $ bool (S.BENot boolExp) boolExp isIn
mkColCompExp
  :: S.Qual -> PGCol -> OpExpG S.SQLExp -> S.BoolExp
mkColCompExp qual lhsCol = \case
  AEQ val          -> equalsBoolExpBuilder lhs val
  ANE val          -> notEqualsBoolExpBuilder lhs val
  AIN  vals        -> mkInOrNotBoolExpBuilder True vals
  ANIN vals        -> mkInOrNotBoolExpBuilder False vals
  AGT val          -> S.BECompare S.SGT lhs val
  ALT val          -> S.BECompare S.SLT lhs val
  AGTE val         -> S.BECompare S.SGTE lhs val
  ALTE val         -> S.BECompare S.SLTE lhs val
  ALIKE val        -> S.BECompare S.SLIKE lhs val
  ANLIKE val       -> S.BECompare S.SNLIKE lhs val
  AILIKE val       -> S.BECompare S.SILIKE lhs val
  ANILIKE val      -> S.BECompare S.SNILIKE lhs val
  ASIMILAR val     -> S.BECompare S.SSIMILAR lhs val
  ANSIMILAR val    -> S.BECompare S.SNSIMILAR lhs val
  AContains val    -> S.BECompare S.SContains lhs val
  AContainedIn val -> S.BECompare S.SContainedIn lhs val
  AHasKey val      -> S.BECompare S.SHasKey lhs val
  AHasKeysAny keys -> S.BECompare S.SHasKeysAny lhs $ toTextArray keys
  AHasKeysAll keys -> S.BECompare S.SHasKeysAll lhs $ toTextArray keys
  ANISNULL         -> S.BENull lhs
  ANISNOTNULL      -> S.BENotNull lhs
  CEQ rhsCol       -> S.BECompare S.SEQ lhs $ mkQCol rhsCol
  CNE rhsCol       -> S.BECompare S.SNE lhs $ mkQCol rhsCol
  CGT rhsCol       -> S.BECompare S.SGT lhs $ mkQCol rhsCol
  CLT rhsCol       -> S.BECompare S.SLT lhs $ mkQCol rhsCol
  CGTE rhsCol      -> S.BECompare S.SGTE lhs $ mkQCol rhsCol
  CLTE rhsCol      -> S.BECompare S.SLTE lhs $ mkQCol rhsCol
  where
    mkQCol = S.SEQIden . S.QIden qual . toIden
    lhs = mkQCol lhsCol

    toTextArray arr =
      S.SETyAnn (S.SEArray $ map (txtEncoder . PGValText) arr) S.textArrType

    -- mkSimpleBoolExpBuilder beF pgColVal =
    --   beF lhs <$> rhsBldr pgColVal

    mkInOrNotBoolExpBuilder isIn rhsExps = do
      let boolExp = S.BEEqualsAny lhs rhsExps
      return $ bool (S.BENot boolExp) boolExp isIn

-- txtRHSBuilder :: (MonadError QErr m) => RHSBuilder m
-- txtRHSBuilder colType = runAesonParser (convToTxt colType)

-- mkColOpSQLExp :: ColOp -> S.SQLExp -> S.SQLExp -> S.BoolExp
-- mkColOpSQLExp colOp =
--   case colOp of
--     CEQ  -> S.BECompare S.SEQ
--     CNE  -> S.BECompare S.SNE
--     CGT  -> S.BECompare S.SGT
--     CLT  -> S.BECompare S.SLT
--     CGTE -> S.BECompare S.SGTE
--     CLTE -> S.BECompare S.SLTE

getColExpDeps :: QualifiedTable -> AnnBoolExpFld a -> [SchemaDependency]
getColExpDeps tn = \case
  AVCol colInfo _ ->
    let cn = pgiName colInfo
    in [SchemaDependency (SOTableObj tn (TOCol cn)) "on_type"]
  AVRel relInfo relBoolExp ->
    let rn = riName relInfo
        relTN = riRTable relInfo
        pd = SchemaDependency (SOTableObj tn (TORel rn)) "on_type"
    in pd : getBoolExpDeps relTN relBoolExp

getBoolExpDeps :: QualifiedTable -> AnnBoolExp a -> [SchemaDependency]
getBoolExpDeps tn =
  foldr (\annFld deps -> getColExpDeps tn annFld <> deps) []
