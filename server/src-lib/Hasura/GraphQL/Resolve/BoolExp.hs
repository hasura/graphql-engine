module Hasura.GraphQL.Resolve.BoolExp
  ( parseBoolExp
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                    as S

type OpExp = OpExpG UnresolvedVal

parseOpExps
  :: (MonadError QErr m)
  => PGColumnType -> AnnInpVal -> m [OpExp]
parseOpExps colTy annVal = do
  opExpsM <- flip withObjectM annVal $ \nt objM -> forM objM $ \obj ->
    forM (OMap.toList obj) $ \(k, v) ->
    case k of
      "_cast"     -> fmap ACast <$> parseCastExpression v

      "_eq"       -> fmap (AEQ True) <$> asOpRhs v
      "_ne"       -> fmap (ANE True) <$> asOpRhs v
      "_neq"      -> fmap (ANE True) <$> asOpRhs v
      "_is_null"  -> resolveIsNull v

      "_in"       -> fmap AIN <$> asPGArray colTy v
      "_nin"      -> fmap ANIN <$> asPGArray colTy v

      "_gt"       -> fmap AGT <$> asOpRhs v
      "_lt"       -> fmap ALT <$> asOpRhs v
      "_gte"      -> fmap AGTE <$> asOpRhs v
      "_lte"      -> fmap ALTE <$> asOpRhs v

      "_like"     -> fmap ALIKE <$> asOpRhs v
      "_nlike"    -> fmap ANLIKE <$> asOpRhs v

      "_ilike"    -> fmap AILIKE <$> asOpRhs v
      "_nilike"   -> fmap ANILIKE <$> asOpRhs v

      "_similar"  -> fmap ASIMILAR <$> asOpRhs v
      "_nsimilar" -> fmap ANSIMILAR <$> asOpRhs v

      -- jsonb related operators
      "_contains"     -> fmap AContains <$> asOpRhs v
      "_contained_in" -> fmap AContainedIn <$> asOpRhs v
      "_has_key"      -> fmap AHasKey <$> asOpRhs v

      "_has_keys_any" -> fmap AHasKeysAny <$> asPGArray (PGColumnScalar PGText) v
      "_has_keys_all" -> fmap AHasKeysAll <$> asPGArray (PGColumnScalar PGText) v

      -- geometry/geography type related operators
      "_st_contains"   -> fmap ASTContains <$> asOpRhs v
      "_st_crosses"    -> fmap ASTCrosses <$> asOpRhs v
      "_st_equals"     -> fmap ASTEquals <$> asOpRhs v
      "_st_intersects" -> fmap ASTIntersects <$> asOpRhs v
      "_st_overlaps"   -> fmap ASTOverlaps <$> asOpRhs v
      "_st_touches"    -> fmap ASTTouches <$> asOpRhs v
      "_st_within"     -> fmap ASTWithin <$> asOpRhs v
      "_st_d_within"   -> asObjectM v >>= mapM parseAsSTDWithinObj

      _ ->
        throw500
          $  "unexpected operator found in opexp of "
          <> showNamedTy nt
          <> ": "
          <> showName k
  return $ catMaybes $ fromMaybe [] opExpsM
  where
    asOpRhs = fmap (fmap UVPG) . asPGColumnValueM

    asPGArray rhsTy v = do
      valsM <- parseMany asPGColumnValue v
      forM valsM $ \vals -> do
        let arrayExp = S.SEArray $ map (txtEncoder . pstValue . _apvValue) vals
        return $ UVSQL $ S.SETyAnn arrayExp $ S.mkTypeAnn $
          -- Safe here because asPGColumnValue ensured all the values are of the right type, but if the
          -- list is empty, we don’t actually have a scalar type to use, so we need to use
          -- unsafePGColumnToRepresentation to create it. (It would be nice to refactor things to
          -- somehow get rid of this.)
          PGTypeArray (unsafePGColumnToRepresentation rhsTy)

    resolveIsNull v = case _aivValue v of
      AGScalar _ Nothing -> return Nothing
      AGScalar _ (Just (PGValBoolean b)) ->
        return $ Just $ bool ANISNOTNULL ANISNULL b
      AGScalar _ _ -> throw500 "boolean value is expected"
      _ -> tyMismatch "pgvalue" v

    parseAsSTDWithinObj obj = do
      distanceVal <- onNothing (OMap.lookup "distance" obj) $
                throw500 "expected \"distance\" input field in st_d_within"
      dist <- UVPG <$> asPGColumnValue distanceVal
      fromVal <- onNothing (OMap.lookup "from" obj) $
                throw500 "expected \"from\" input field in st_d_within"
      from <- UVPG <$> asPGColumnValue fromVal
      case colTy of
        PGColumnScalar PGGeography -> do
          useSpheroidVal <-
            onNothing (OMap.lookup "use_spheroid" obj) $
            throw500 "expected \"use_spheroid\" input field in st_d_within"
          useSpheroid <- UVPG <$> asPGColumnValue useSpheroidVal
          return $ ASTDWithinGeog $ DWithinGeogOp dist from useSpheroid
        PGColumnScalar PGGeometry ->
          return $ ASTDWithinGeom $ DWithinGeomOp dist from
        _ -> throw500 "expected PGGeometry/PGGeography column for st_d_within"

parseCastExpression
  :: (MonadError QErr m)
  => AnnInpVal -> m (Maybe (CastExp UnresolvedVal))
parseCastExpression =
  withObjectM $ \_ objM -> forM objM $ \obj -> do
    targetExps <- forM (OMap.toList obj) $ \(targetTypeName, castedComparisonExpressionInput) -> do
      let targetType = txtToPgColTy $ G.unName targetTypeName
      castedComparisonExpressions <- parseOpExps (PGColumnScalar targetType) castedComparisonExpressionInput
      return (targetType, castedComparisonExpressions)
    return $ Map.fromList targetExps

parseColExp
  :: ( MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     )
  => G.NamedType -> G.Name -> AnnInpVal
  -> m (AnnBoolExpFld UnresolvedVal)
parseColExp nt n val = do
  fldInfo <- getFldInfo nt n
  case fldInfo of
    Left pgColInfo -> do
      opExps <- parseOpExps (pgiType pgColInfo) val
      return $ AVCol pgColInfo opExps
    Right (relInfo, _, permExp, _) -> do
      relBoolExp <- parseBoolExp val
      return $ AVRel relInfo $ andAnnBoolExps relBoolExp $
        fmapAnnBoolExp partialSQLExpToUnresolvedVal permExp

parseBoolExp
  :: ( MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     )
  => AnnInpVal -> m (AnnBoolExp UnresolvedVal)
parseBoolExp annGVal = do
  boolExpsM <-
    flip withObjectM annGVal
      $ \nt objM -> forM objM $ \obj -> forM (OMap.toList obj) $ \(k, v) -> if
          | k == "_or"  -> BoolOr . fromMaybe []
                           <$> parseMany parseBoolExp v
          | k == "_and" -> BoolAnd . fromMaybe []
                           <$> parseMany parseBoolExp v
          | k == "_not" -> BoolNot <$> parseBoolExp v
          | otherwise   -> BoolFld <$> parseColExp nt k v
  return $ BoolAnd $ fromMaybe [] boolExpsM
