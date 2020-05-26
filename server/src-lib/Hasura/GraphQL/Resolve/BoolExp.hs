module Hasura.GraphQL.Resolve.BoolExp
  ( parseBoolExp
  , pgColValToBoolExp
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

parseOpExps :: (MonadReusability m, MonadError QErr m) => PGColumnType -> AnnInpVal -> m [OpExp]
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
      "_st_d_within"   -> parseAsObjectM v parseAsSTDWithinObj

      -- raster type related operators
      "_st_intersects_rast"       -> fmap ASTIntersectsRast <$> asOpRhs v
      "_st_intersects_nband_geom" -> parseAsObjectM v parseAsSTIntersectsNbandGeomObj
      "_st_intersects_geom_nband" -> parseAsObjectM v parseAsSTIntersectsGeomNbandObj

      _ ->
        throw500
          $  "unexpected operator found in opexp of "
          <> showNamedTy nt
          <> ": "
          <> showName k
  return $ catMaybes $ fromMaybe [] opExpsM
  where
    asOpRhs = fmap (fmap mkParameterizablePGValue) . asPGColumnValueM

    parseAsObjectM v f = asObjectM v >>= mapM f

    asPGArray rhsTy v = do
      valsM <- parseMany (openOpaqueValue <=< asPGColumnValue) v
      forM valsM $ \vals -> do
        let arrayExp = S.SEArray $ map (txtEncoder . pstValue . _apvValue) vals
        return $ UVSQL $ S.SETyAnn arrayExp $ S.mkTypeAnn $
          -- Safe here because asPGColumnValue ensured all the values are of the right type, but if the
          -- list is empty, we don’t actually have a scalar type to use, so we need to use
          -- unsafePGColumnToRepresentation to create it. (It would be nice to refactor things to
          -- somehow get rid of this.)
          PGTypeArray (unsafePGColumnToRepresentation rhsTy)

    resolveIsNull v = asPGColumnValueM v >>= traverse openOpaqueValue >>= \case
      Nothing -> pure Nothing
      Just annPGVal -> case pstValue $ _apvValue annPGVal of
        PGValBoolean b -> pure . Just $ bool ANISNOTNULL ANISNULL b
        _              -> throw500 "boolean value is expected"

    parseAsSTDWithinObj obj = do
      distanceVal <- onNothing (OMap.lookup "distance" obj) $
                throw500 "expected \"distance\" input field in st_d_within"
      dist <- mkParameterizablePGValue <$> asPGColumnValue distanceVal
      fromVal <- onNothing (OMap.lookup "from" obj) $
                throw500 "expected \"from\" input field in st_d_within"
      from <- mkParameterizablePGValue <$> asPGColumnValue fromVal
      case colTy of
        PGColumnScalar PGGeography -> do
          useSpheroidVal <-
            onNothing (OMap.lookup "use_spheroid" obj) $
            throw500 "expected \"use_spheroid\" input field in st_d_within"
          useSpheroid <- mkParameterizablePGValue <$> asPGColumnValue useSpheroidVal
          return $ ASTDWithinGeog $ DWithinGeogOp dist from useSpheroid
        PGColumnScalar PGGeometry ->
          return $ ASTDWithinGeom $ DWithinGeomOp dist from
        _ -> throw500 "expected PGGeometry/PGGeography column for st_d_within"

    parseAsSTIntersectsNbandGeomObj obj = do
      nbandVal <- onNothing (OMap.lookup "nband" obj) $
                  throw500 "expected \"nband\" input field"
      nband <- mkParameterizablePGValue <$> asPGColumnValue nbandVal
      geommin <- parseGeommin obj
      return $ ASTIntersectsNbandGeom $ STIntersectsNbandGeommin nband geommin

    parseAsSTIntersectsGeomNbandObj obj = do
      nbandMM <- fmap (fmap mkParameterizablePGValue) <$>
        traverse asPGColumnValueM (OMap.lookup "nband" obj)
      geommin <- parseGeommin obj
      return $ ASTIntersectsGeomNband $ STIntersectsGeomminNband geommin $ join nbandMM

    parseGeommin obj = do
      geomminVal <- onNothing (OMap.lookup "geommin" obj) $
                    throw500 "expected \"geommin\" input field"
      mkParameterizablePGValue <$> asPGColumnValue geomminVal

parseCastExpression
  :: (MonadReusability m, MonadError QErr m)
  => AnnInpVal -> m (Maybe (CastExp UnresolvedVal))
parseCastExpression =
  withObjectM $ \_ objM -> forM objM $ \obj -> do
    targetExps <- forM (OMap.toList obj) $ \(targetTypeName, castedComparisonExpressionInput) -> do
      let targetType = textToPGScalarType $ G.unName targetTypeName
      castedComparisonExpressions <- parseOpExps (PGColumnScalar targetType) castedComparisonExpressionInput
      return (targetType, castedComparisonExpressions)
    return $ Map.fromList targetExps

parseColExp
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     )
  => G.NamedType -> G.Name -> AnnInpVal
  -> m (AnnBoolExpFld UnresolvedVal)
parseColExp nt n val = do
  fldInfo <- getFldInfo nt n
  case fldInfo of
    RFPGColumn pgColInfo -> do
      opExps <- parseOpExps (pgiType pgColInfo) val
      return $ AVCol pgColInfo opExps
    RFRelationship (RelationshipField relInfo _ _ permExp _)-> do
      relBoolExp <- parseBoolExp val
      return $ AVRel relInfo $ andAnnBoolExps relBoolExp $
        fmapAnnBoolExp partialSQLExpToUnresolvedVal permExp
    RFComputedField _ -> throw500
          "computed fields are not allowed in bool_exp"
    RFNodeId _ _      -> throw500
      "node id is not allowed in bool_exp"

parseBoolExp
  :: ( MonadReusability m
     , MonadError QErr m
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

type PGColValMap = Map.HashMap G.Name AnnInpVal

pgColValToBoolExp
  :: (MonadReusability m, MonadError QErr m)
  => PGColArgMap -> PGColValMap -> m AnnBoolExpUnresolved
pgColValToBoolExp colArgMap colValMap = do
  colExps <- forM colVals $ \(name, val) ->
    BoolFld <$> do
      opExp <- AEQ True . mkParameterizablePGValue <$> asPGColumnValue val
      colInfo <- onNothing (Map.lookup name colArgMap) $
        throw500 $ "column name " <> showName name
        <> " not found in column arguments map"
      return $ AVCol colInfo [opExp]
  return $ BoolAnd colExps
  where
    colVals = Map.toList colValMap
