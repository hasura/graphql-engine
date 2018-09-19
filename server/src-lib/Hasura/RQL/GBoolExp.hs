{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Hasura.RQL.GBoolExp where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML      as S

import           Data.Aeson

import qualified Data.HashMap.Strict as M
import qualified Data.Text.Extended  as T

data AnnValOpExpG a
  = AEQ !a
  | ANE !a

  | AIN  ![a]
  | ANIN ![a]

  | AGT !a
  | ALT !a
  | AGTE !a
  | ALTE !a

  | ALIKE !a -- LIKE
  | ANLIKE !a -- NOT LIKE

  | AILIKE !a -- ILIKE, case insensitive
  | ANILIKE !a-- NOT ILIKE, case insensitive

  | ASIMILAR !a -- similar, regex
  | ANSIMILAR !a-- not similar, regex

  | AContains !a
  | AContainedIn !a
  | AHasKey !a
  | AHasKeysAny [Text]
  | AHasKeysAll [Text]

  | ANISNULL -- IS NULL
  | ANISNOTNULL -- IS NOT NULL

  deriving (Eq, Show)

data OpExpG a
  = OEVal !(AnnValOpExpG a)
  | OECol !ColOp !PGCol
  deriving (Show, Eq)

type OpExpJ = OpExpG Value
type OpExp = OpExpG (PGColType, PGColValue)

data AnnValG a
  = AVCol !PGColInfo !a
  | AVRel !RelInfo !(GBoolExp (AnnValG a)) S.BoolExp
  deriving (Show, Eq)

type AnnValS = AnnValG [OpExpG S.SQLExp]
type AnnValO a = AnnValG [OpExpG a]
type AnnVal = AnnValO (PGColType, PGColValue)

type AnnValJ = AnnValG [OpExpJ]

type AnnSQLBoolExp = AnnValG S.BoolExp

data ColOp
  = CEQ
  | CNE
  | CGT
  | CLT
  | CGTE
  | CLTE
  deriving (Eq)

instance Show ColOp where
  show CEQ  = "$ceq"
  show CNE  = "$cne"

  show CGT  = "$cgt"
  show CLT  = "$clt"
  show CGTE = "$cgte"
  show CLTE = "$clte"

data RQLOp
  = REQ -- equals
  | RNE -- <>

  | RIN  -- in an array
  | RNIN -- not in an array

  | RGT    -- >
  | RLT    -- <
  | RGTE   -- >=
  | RLTE   -- <=

  | RLIKE  -- LIKE
  | RNLIKE  -- NOT LIKE

  | RILIKE  -- ILIKE, case insensitive
  | RNILIKE -- NOT ILIKE, case insensitive

  | RSIMILAR  -- similar, regex
  | RNSIMILAR -- not similar, regex

  | RISNULL -- is null

  deriving (Eq)

instance Show RQLOp where
  show REQ       = "$eq"
  show RNE       = "$ne"

  show RIN       = "$in"
  show RNIN      = "$nin"

  show RGT       = "$gt"
  show RLT       = "$lt"
  show RGTE      = "$gte"
  show RLTE      = "$lte"

  show RLIKE     = "$like"
  show RNLIKE    = "$nlike"

  show RILIKE    = "$ilike"
  show RNILIKE   = "$nilike"

  show RSIMILAR  = "$similar"
  show RNSIMILAR = "$nsimilar"

  show RISNULL   = "$is_null"

instance DQuote RQLOp where
  dquoteTxt op = T.pack $ show op

parseOp :: (MonadError QErr m) => T.Text -> m (Either RQLOp ColOp)
parseOp opStr = case opStr of
  "$eq"       -> return $ Left REQ
  "_eq"       -> return $ Left REQ
  "$ne"       -> return $ Left RNE
  "_ne"       -> return $ Left RNE
  "$neq"      -> return $ Left RNE
  "_neq"      -> return $ Left RNE

  "$in"       -> return $ Left RIN
  "_in"       -> return $ Left RIN
  "$nin"      -> return $ Left RNIN
  "_nin"      -> return $ Left RNIN

  "$gt"       -> return $ Left RGT
  "_gt"       -> return $ Left RGT
  "$lt"       -> return $ Left RLT
  "_lt"       -> return $ Left RLT
  "$gte"      -> return $ Left RGTE
  "_gte"      -> return $ Left RGTE
  "$lte"      -> return $ Left RLTE
  "_lte"      -> return $ Left RLTE

  "$like"     -> return $ Left RLIKE
  "_like"     -> return $ Left RLIKE
  "$nlike"    -> return $ Left RNLIKE
  "_nlike"    -> return $ Left RNLIKE

  "$ilike"    -> return $ Left RILIKE
  "_ilike"    -> return $ Left RILIKE
  "$nilike"   -> return $ Left RNILIKE
  "_nilike"   -> return $ Left RNILIKE

  "$similar"  -> return $ Left RSIMILAR
  "_similar"  -> return $ Left RSIMILAR
  "$nsimilar" -> return $ Left RNSIMILAR
  "_nsimilar" -> return $ Left RNSIMILAR

  "$is_null"  -> return $ Left RISNULL
  "_is_null"  -> return $ Left RISNULL

  "$ceq"      -> return $ Right CEQ
  "_ceq"      -> return $ Right CEQ
  "$cne"      -> return $ Right CNE
  "_cne"      -> return $ Right CNE
  "$cneq"     -> return $ Right CNE
  "_cneq"     -> return $ Right CNE

  "$cgt"      -> return $ Right CGT
  "_cgt"      -> return $ Right CGT
  "$clt"      -> return $ Right CLT
  "_clt"      -> return $ Right CLT
  "$cgte"     -> return $ Right CGTE
  "_cgte"     -> return $ Right CGTE
  "$clte"     -> return $ Right CLTE
  "_clte"     -> return $ Right CLTE

  x           -> throw400 UnexpectedPayload $ "Unknown operator : " <> x

isRQLOp :: T.Text -> Bool
isRQLOp t = case runIdentity . runExceptT $ parseOp t of
  Left _  -> False
  Right r -> either (const True) (const False) r

type ValueParser m a = PGColType -> Value -> m a

parseAnnOpExpG
  :: (MonadError QErr m)
  => (PGColType -> Value -> m a)
  -> RQLOp -> PGColType -> Value -> m (AnnValOpExpG a)
parseAnnOpExpG parser op ty val = case op of
  REQ       -> AEQ <$> parseOne -- equals
  RNE       -> ANE <$> parseOne -- <>
  RIN       -> AIN <$> parseMany -- in an array
  RNIN      -> ANIN <$> parseMany -- not in an array
  RGT       -> AGT <$> parseOne -- >
  RLT       -> ALT <$> parseOne -- <
  RGTE      -> AGTE <$> parseOne -- >=
  RLTE      -> ALTE <$> parseOne -- <=
  RLIKE     -> ALIKE <$> parseOne -- LIKE
  RNLIKE    -> ANLIKE <$> parseOne -- NOT LIKE
  RILIKE    -> AILIKE <$> parseOne -- ILIKE, case insensitive
  RNILIKE   -> ANILIKE <$> parseOne -- NOT ILIKE, case insensitive
  RSIMILAR  -> ASIMILAR <$> parseOne -- similar, regex
  RNSIMILAR -> ANSIMILAR <$> parseOne -- not similar, regex
  RISNULL   -> bool ANISNOTNULL ANISNULL -- is null
               <$> decodeValue val
  where
    parseOne = parser ty val
      -- runAesonParser (parsePGValue ty) val
    parseMany = do
      vals <- runAesonParser parseJSON val
      indexedForM vals (parser ty)

parseOpExps
  :: (MonadError QErr m)
  => ValueParser m a
  -> FieldInfoMap
  -> PGColInfo
  -> Value
  -> m [OpExpG a]
parseOpExps valParser cim (PGColInfo cn colTy _) (Object o) =
  forM (M.toList o) $ \(k, v) -> do
  op <- parseOp k
  case (op, v) of
    (Left rqlOp, _) -> do
      modifyErr (cn <<>) $ getOpTypeChecker rqlOp colTy
      annValOp <- withPathK (T.pack $ show rqlOp) $
                  parseAnnOpExpG valParser rqlOp colTy v
      return $ OEVal annValOp
    (Right colOp, String c) -> do
      let pgCol  = PGCol c
          errMsg = "column operators can only compare postgres columns"
      rhsType <- askPGType cim pgCol errMsg
      when (colTy /= rhsType) $
        throw400 UnexpectedPayload $
        "incompatible column types : " <> cn <<> ", " <>> pgCol
      return $ OECol colOp pgCol
    (Right _, _) -> throw400 UnexpectedPayload "expecting a string for column operator"
parseOpExps valParser _ (PGColInfo _ colTy _) val = do
  annValOp <- parseAnnOpExpG valParser REQ colTy val
  return [OEVal annValOp]

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

validOnAllTypes :: (MonadError QErr m) => OpTypeChecker m
validOnAllTypes _ = return ()

getOpTypeChecker :: (MonadError QErr m) => RQLOp -> OpTypeChecker m
getOpTypeChecker REQ       = validOnAllTypes
getOpTypeChecker RNE       = validOnAllTypes
getOpTypeChecker RIN       = validOnAllTypes
getOpTypeChecker RNIN      = validOnAllTypes
getOpTypeChecker RGT       = validOnAllTypes
getOpTypeChecker RLT       = validOnAllTypes
getOpTypeChecker RGTE      = validOnAllTypes
getOpTypeChecker RLTE      = validOnAllTypes
getOpTypeChecker RLIKE     = textOnlyOp
getOpTypeChecker RNLIKE    = textOnlyOp
getOpTypeChecker RILIKE    = textOnlyOp
getOpTypeChecker RNILIKE   = textOnlyOp
getOpTypeChecker RSIMILAR  = textOnlyOp
getOpTypeChecker RNSIMILAR = textOnlyOp
getOpTypeChecker RISNULL   = validOnAllTypes

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

mapBoolExp :: (Monad m)
           => (a -> m b)
           -> GBoolExp a -> m (GBoolExp b)
mapBoolExp f (BoolAnd bes)    = BoolAnd <$> mapM (mapBoolExp f) bes
mapBoolExp f (BoolOr bes)     = BoolOr  <$> mapM (mapBoolExp f) bes
mapBoolExp f (BoolCol ce)     = BoolCol <$> f ce
mapBoolExp f (BoolNot notExp) = BoolNot <$> mapBoolExp f notExp

annBoolExp
  :: (QErrM m, CacheRM m)
  => ValueParser m a
  -> FieldInfoMap
  -> GBoolExp ColExp
  -> m (GBoolExp (AnnValG [OpExpG a]))
annBoolExp valParser cim = \case
  (BoolAnd bes)    -> BoolAnd <$> mapM (annBoolExp valParser cim) bes
  (BoolOr bes)     -> BoolOr  <$> mapM (annBoolExp valParser cim) bes
  (BoolCol ce)     -> BoolCol <$> annColExp valParser cim ce
  (BoolNot notExp) -> BoolNot <$> annBoolExp valParser cim notExp

annColExp
  :: (QErrM m, CacheRM m)
  => ValueParser m a
  -> FieldInfoMap
  -> ColExp
  -> m (AnnValG [OpExpG a])
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
      return $ AVRel relInfo annRelBoolExp $ S.BELit True

type BoolExpBuilder m a = S.SQLExp -> AnnValOpExpG a -> m S.BoolExp

convBoolRhs
  :: (Monad m)
  => BoolExpBuilder m a -> S.Qual
  -> GBoolExp (AnnValO a) -> m (GBoolExp AnnSQLBoolExp)
convBoolRhs vp tq =
  traverse (convColRhs vp tq )

convColRhs
  :: (Monad m)
  => BoolExpBuilder m a
  -> S.Qual -> AnnValO a -> m (AnnValG S.BoolExp)
convColRhs bExpBuilder tableQual annVal = case annVal of
  AVCol pci@(PGColInfo cn _ _) opExps -> do
    let qualColExp = S.SEQIden $ S.QIden tableQual (toIden cn)
    bExps <- forM opExps $ \case
      OEVal annOpValExp -> bExpBuilder qualColExp annOpValExp
      OECol op rCol -> do
        let rhsColExp = S.SEQIden $ S.QIden tableQual (toIden rCol)
        return $ mkColOpSQLExp op qualColExp rhsColExp
    -- And them all
    return $ AVCol pci $ foldr (S.BEBin S.AndOp) (S.BELit True) bExps

  AVRel ri@(RelInfo _ _ colMapping relTN _ _) nesAnn fltr -> do
    -- Convert the where clause on the relationship
    annRelBoolExp <- convBoolRhs bExpBuilder (S.mkQual relTN) nesAnn
    let backCompExp  = foldr (S.BEBin S.AndOp) (S.BELit True) $
          flip map colMapping $ \(lCol, rCol) ->
          S.BECompare S.SEQ (S.mkSIdenExp rCol)
          (S.SEQIden $ S.QIden tableQual (toIden lCol))
    return $ AVRel ri annRelBoolExp $ S.BEBin S.AndOp fltr backCompExp

cBoolExp
  :: GBoolExp AnnSQLBoolExp
  -> S.BoolExp
cBoolExp be =
  runIdentity $ flip foldBoolExp be $ \ace ->
  return $ cColExp ace

cColExp
  :: AnnSQLBoolExp
  -> S.BoolExp
cColExp annVal = case annVal of
  AVCol _ be -> be
  AVRel (RelInfo _ _ _ relTN _ _) nesAnn backCompExp -> do
    -- Convert the where clause on the relationship
    let annRelBoolExp = cBoolExp nesAnn
        innerBoolExp = S.BEBin S.AndOp backCompExp annRelBoolExp
    S.mkExists relTN innerBoolExp

inBoolExpBuilder :: S.SQLExp -> [S.SQLExp] -> S.BoolExp
inBoolExpBuilder qualColExp rhsExps =
  foldr (S.BEBin S.OrOp) (S.BELit False) eqExps
  where
    eqExps = map (equalsBoolExpBuilder qualColExp) rhsExps

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

-- binExpBuilder
--   :: (Monad m)
--   => BoolExpBuilder m PGColValue
-- binExpBuilder =
--   mkBoolExpBuilder

mkBoolExpBuilder
  :: (Monad m)
  => (a -> m S.SQLExp)
  -> BoolExpBuilder m a
mkBoolExpBuilder rhsBldr lhs = \case
  AEQ val          -> mkSimpleBoolExpBuilder equalsBoolExpBuilder val
  ANE val          -> mkSimpleBoolExpBuilder notEqualsBoolExpBuilder val
  AIN  vals        -> mkInOrNotBoolExpBuilder True vals
  ANIN vals        -> mkInOrNotBoolExpBuilder False vals
  AGT val          -> mkSimpleBoolExpBuilder (S.BECompare S.SGT) val
  ALT val          -> mkSimpleBoolExpBuilder (S.BECompare S.SLT) val
  AGTE val         -> mkSimpleBoolExpBuilder (S.BECompare S.SGTE) val
  ALTE val         -> mkSimpleBoolExpBuilder (S.BECompare S.SLTE) val
  ALIKE val        -> mkSimpleBoolExpBuilder (S.BECompare S.SLIKE) val
  ANLIKE val       -> mkSimpleBoolExpBuilder (S.BECompare S.SNLIKE) val
  AILIKE val       -> mkSimpleBoolExpBuilder (S.BECompare S.SILIKE) val
  ANILIKE val      -> mkSimpleBoolExpBuilder (S.BECompare S.SNILIKE) val
  ASIMILAR val     -> mkSimpleBoolExpBuilder (S.BECompare S.SSIMILAR) val
  ANSIMILAR val    -> mkSimpleBoolExpBuilder (S.BECompare S.SNSIMILAR) val
  AContains val    -> mkSimpleBoolExpBuilder (S.BECompare S.SContains) val
  AContainedIn val -> mkSimpleBoolExpBuilder (S.BECompare S.SContainedIn) val
  AHasKey val      -> mkSimpleBoolExpBuilder (S.BECompare S.SHasKey) val
  AHasKeysAny keys -> return $ S.BECompare S.SHasKeysAny lhs $ toTextArray keys
  AHasKeysAll keys -> return $ S.BECompare S.SHasKeysAll lhs $ toTextArray keys
  ANISNULL         -> return $ S.BENull lhs
  ANISNOTNULL      -> return $ S.BENotNull lhs
  where
    toTextArray arr =
      S.SETyAnn (S.SEArray $ map (txtEncoder . PGValText) arr) S.textArrType

    mkSimpleBoolExpBuilder beF pgColVal =
      beF lhs <$> rhsBldr pgColVal

    mkInOrNotBoolExpBuilder isIn arrVals = do
      rhsExps <- mapM rhsBldr arrVals
      let boolExp = inBoolExpBuilder lhs rhsExps
      return $ bool (S.BENot boolExp) boolExp isIn

-- txtRHSBuilder :: (MonadError QErr m) => RHSBuilder m
-- txtRHSBuilder colType = runAesonParser (convToTxt colType)

mkColOpSQLExp :: ColOp -> S.SQLExp -> S.SQLExp -> S.BoolExp
mkColOpSQLExp colOp =
  case colOp of
    CEQ  -> S.BECompare S.SEQ
    CNE  -> S.BECompare S.SNE
    CGT  -> S.BECompare S.SGT
    CLT  -> S.BECompare S.SLT
    CGTE -> S.BECompare S.SGTE
    CLTE -> S.BECompare S.SLTE

getColExpDeps :: QualifiedTable -> AnnValG a -> [SchemaDependency]
getColExpDeps tn (AVCol pgCI _) =
  [SchemaDependency (SOTableObj tn (TOCol $ pgiName pgCI)) "on_type"]
getColExpDeps tn (AVRel relInfo nesAnn _) =
  pd : getBoolExpDeps (riRTable relInfo) nesAnn
  where
    pd = SchemaDependency (SOTableObj tn (TORel $ riName relInfo)) "on_type"

getBoolExpDeps :: QualifiedTable -> GBoolExp (AnnValG a) -> [SchemaDependency]
getBoolExpDeps tn (BoolAnd exps) =
  mconcat $ map (getBoolExpDeps tn) exps
getBoolExpDeps tn (BoolOr exps) =
  mconcat $ map (getBoolExpDeps tn) exps
getBoolExpDeps tn (BoolCol colExp) =
  getColExpDeps tn colExp
getBoolExpDeps tn (BoolNot notExp) =
  getBoolExpDeps tn notExp
