module Hasura.GraphQL.Resolve.BoolExp
  ( parseBoolExp
  , pgColValToBoolExp
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.ContextTypes
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

type OpExp = OpExpG AnnPGVal

parseOpExps
  :: (MonadError QErr m)
  => PGColType -> AnnInpVal -> m [OpExp]
parseOpExps colTy annVal = do
  opExpsM <- flip withObjectM annVal $ \nt objM -> forM objM $ \obj ->
    forM (OMap.toList obj) $ \(k, v) -> do
    case k of
      "_eq"       -> fmap (AEQ True) <$> asPGColValM v
      "_ne"       -> fmap (ANE True) <$> asPGColValM v
      "_neq"      -> fmap (ANE True) <$> asPGColValM v
      "_is_null"  -> resolveIsNull v

      "_in"       -> fmap (AIN . catMaybes) <$> parseMany asPGColValM v
      "_nin"      -> fmap (ANIN . catMaybes) <$> parseMany asPGColValM v

      "_gt"       -> fmap AGT <$> asPGColValM v
      "_lt"       -> fmap ALT <$> asPGColValM v
      "_gte"      -> fmap AGTE <$> asPGColValM v
      "_lte"      -> fmap ALTE <$> asPGColValM v

      "_like"     -> fmap ALIKE <$> asPGColValM v
      "_nlike"    -> fmap ANLIKE <$> asPGColValM v

      "_ilike"    -> fmap AILIKE <$> asPGColValM v
      "_nilike"   -> fmap ANILIKE <$> asPGColValM v

      "_similar"  -> fmap ASIMILAR <$> asPGColValM v
      "_nsimilar" -> fmap ANSIMILAR <$> asPGColValM v

      -- jsonb related operators
      "_contains"     -> fmap AContains <$> asPGColValM v
      "_contained_in" -> fmap AContainedIn <$> asPGColValM v
      "_has_key"      -> fmap AHasKey <$> asPGColValM v
      "_has_keys_any" -> fmap AHasKeysAny <$> parseMany asPGColText v
      "_has_keys_all" -> fmap AHasKeysAll <$> parseMany asPGColText v

      -- geometry/geography type related operators
      "_st_contains"   -> fmap ASTContains <$> asPGColValM v
      "_st_crosses"    -> fmap ASTCrosses <$> asPGColValM v
      "_st_equals"     -> fmap ASTEquals <$> asPGColValM v
      "_st_intersects" -> fmap ASTIntersects <$> asPGColValM v
      "_st_overlaps"   -> fmap ASTOverlaps <$> asPGColValM v
      "_st_touches"    -> fmap ASTTouches <$> asPGColValM v
      "_st_within"     -> fmap ASTWithin <$> asPGColValM v
      "_st_d_within"   -> asObjectM v >>= mapM parseAsSTDWithinObj

      _ ->
        throw500
          $  "unexpected operator found in opexp of "
          <> showNamedTy nt
          <> ": "
          <> showName k
  return $ catMaybes $ fromMaybe [] opExpsM
  where
    resolveIsNull v = case _aivValue v of
      AGScalar _ Nothing -> return Nothing
      AGScalar _ (Just (PGValBoolean b)) ->
        return $ Just $ bool ANISNOTNULL ANISNULL b
      AGScalar _ _ -> throw500 "boolean value is expected"
      _ -> tyMismatch "pgvalue" v

    parseAsSTDWithinObj obj = do
      distanceVal <- onNothing (OMap.lookup "distance" obj) $
                throw500 "expected \"distance\" input field in st_d_within"
      dist <- asPGColVal distanceVal
      fromVal <- onNothing (OMap.lookup "from" obj) $
                throw500 "expected \"from\" input field in st_d_within"
      from <- asPGColVal fromVal
      case colTy of
        PGGeography -> do
          useSpheroidVal <- onNothing (OMap.lookup "use_spheroid" obj) $
                    throw500 "expected \"use_spheroid\" input field in st_d_within"
          useSpheroid <- asPGColVal useSpheroidVal
          return $ ASTDWithinGeog $ DWithinGeogOp dist from useSpheroid
        PGGeometry -> do
          return $ ASTDWithinGeom $ DWithinGeomOp dist from
        _ -> throw500 "expected PGGeometry/PGGeography column for st_d_within"

parseAsEqOp
  :: (MonadError QErr m)
  => AnnInpVal -> m [OpExp]
parseAsEqOp annVal = do
  annValOpExp <- AEQ True <$> asPGColVal annVal
  return [annValOpExp]

parseColExp
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => PrepFn m -> G.NamedType -> G.Name -> AnnInpVal
  -> m AnnBoolExpFldSQL
parseColExp f nt n val = do
  fldInfo <- getFldInfo nt n
  case fldInfo of
    SFldPGCol pgColInfo -> do
      opExps <- parseOpExps (pgiType pgColInfo) val
      AVCol pgColInfo <$> traverse (traverse f) opExps
    SFldRel (RelSelCtx relInfo permExp _ _) -> do
      relBoolExp <- parseBoolExp f val
      return $ AVRel relInfo $ andAnnBoolExps relBoolExp permExp
    SFldCompCol _ _ -> throw500 "unexpected computed column in boolexp"

parseBoolExp
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => PrepFn m -> AnnInpVal -> m AnnBoolExpSQL
parseBoolExp f annGVal = do
  boolExpsM <-
    flip withObjectM annGVal
      $ \nt objM -> forM objM $ \obj -> forM (OMap.toList obj) $ \(k, v) -> if
          | k == "_or"  -> BoolOr . fromMaybe []
                           <$> parseMany (parseBoolExp f) v
          | k == "_and" -> BoolAnd . fromMaybe []
                           <$> parseMany (parseBoolExp f) v
          | k == "_not" -> BoolNot <$> parseBoolExp f v
          | otherwise   -> BoolFld <$> parseColExp f nt k v
  return $ BoolAnd $ fromMaybe [] boolExpsM

type PGColValMap = Map.HashMap G.Name AnnInpVal

pgColValToBoolExp
  :: (MonadError QErr m)
  => PrepFn m -> PGColArgMap -> PGColValMap -> m AnnBoolExpSQL
pgColValToBoolExp f colArgMap colValMap = do
  colExps <- forM colVals $ \(name, val) ->
    BoolFld <$> do
      opExps <- parseAsEqOp val
      colInfo <- onNothing (Map.lookup name colArgMap) $
        throw500 $ "column name " <> showName name
        <> " not found in column arguments map"
      AVCol colInfo <$> traverse (traverse f) opExps
  return $ BoolAnd colExps
  where
    colVals = Map.toList colValMap
