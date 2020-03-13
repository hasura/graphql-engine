module Hasura.GraphQL.Schema.BoolExp
  ( geoInputTypes
  , rasterIntersectsInputTypes
  , mkCompExpInp

  , mkBoolExpTy
  , mkBoolExpInp
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

typeToDescription :: G.NamedType -> G.Description
typeToDescription = G.Description . G.unName . G.unNamedType

mkCompExpTy :: PGColumnType -> G.NamedType
mkCompExpTy = addTypeSuffix "_comparison_exp" . mkColumnType

mkCastExpTy :: PGColumnType -> G.NamedType
mkCastExpTy = addTypeSuffix "_cast_exp" . mkColumnType

-- TODO(shahidhk) this should ideally be st_d_within_geometry
{-
input st_d_within_input {
  distance: Float!
  from: geometry!
}
-}
stDWithinGeometryInpTy :: G.NamedType
stDWithinGeometryInpTy = G.NamedType "st_d_within_input"

{-
input st_d_within_geography_input {
  distance: Float!
  from: geography!
  use_spheroid: Bool!
}
-}
stDWithinGeographyInpTy :: G.NamedType
stDWithinGeographyInpTy = G.NamedType "st_d_within_geography_input"


-- | Makes an input type declaration for the @_cast@ field of a comparison expression.
-- (Currently only used for casting between geometry and geography types.)
mkCastExpressionInputType :: PGColumnType -> [PGColumnType] -> InpObjTyInfo
mkCastExpressionInputType sourceType targetTypes =
  mkHsraInpTyInfo (Just description) (mkCastExpTy sourceType) (fromInpValL targetFields)
  where
    description = mconcat
      [ "Expression to compare the result of casting a column of type "
      , typeToDescription $ mkColumnType sourceType
      , ". Multiple cast targets are combined with logical 'AND'."
      ]
    targetFields = map targetField targetTypes
    targetField targetType = InpValInfo
      Nothing
      (G.unNamedType $ mkColumnType targetType)
      Nothing
      (G.toGT $ mkCompExpTy targetType)

--- | make compare expression input type
mkCompExpInp :: PGColumnType -> InpObjTyInfo
mkCompExpInp colTy =
  InpObjTyInfo (Just tyDesc) (mkCompExpTy colTy) (fromInpValL $ concat
  [ map (mk colGqlType) eqOps
  , guard (isScalarWhere (/= PGRaster)) *> map (mk colGqlType) compOps
  , map (mk $ G.toLT $ G.toNT colGqlType) listOps
  , guard (isScalarWhere isStringType) *> map (mk $ mkScalarTy PGText) stringOps
  , guard (isScalarWhere (== PGJSONB)) *> map opToInpVal jsonbOps
  , guard (isScalarWhere (== PGGeometry)) *>
      (stDWithinGeoOpInpVal stDWithinGeometryInpTy : map geoOpToInpVal (geoOps ++ geomOps))
  , guard (isScalarWhere (== PGGeography)) *>
      (stDWithinGeoOpInpVal stDWithinGeographyInpTy : map geoOpToInpVal geoOps)
  , [InpValInfo Nothing "_is_null" Nothing $ G.TypeNamed (G.Nullability True) $ G.NamedType "Boolean"]
  , castOpInputValues
  , guard (isScalarWhere (== PGRaster)) *> map opToInpVal rasterOps
  ]) TLHasuraType
  where
    colGqlType = mkColumnType colTy
    colTyDesc = typeToDescription colGqlType
    tyDesc =
      "expression to compare columns of type " <> colTyDesc
        <> ". All fields are combined with logical 'AND'."
    isScalarWhere = flip isScalarColumnWhere colTy
    mk t n = InpValInfo Nothing n Nothing $ G.toGT t

    -- colScalarListTy = GA.GTList colGTy
    eqOps =
       ["_eq", "_neq"]
    compOps =
      ["_gt", "_lt", "_gte", "_lte"]

    listOps =
      [ "_in", "_nin" ]
    -- TODO
    -- columnOps =
    --   [ "_ceq", "_cneq", "_cgt", "_clt", "_cgte", "_clte"]
    stringOps =
      [ "_like", "_nlike", "_ilike", "_nilike"
      , "_similar", "_nsimilar"
      ]

    opToInpVal (opName, ty, desc) = InpValInfo (Just desc) opName Nothing ty

    jsonbOps =
      [ ( "_contains"
        , G.toGT $ mkScalarTy PGJSONB
        , "does the column contain the given json value at the top level"
        )
      , ( "_contained_in"
        , G.toGT $ mkScalarTy PGJSONB
        , "is the column contained in the given json value"
        )
      , ( "_has_key"
        , G.toGT $ mkScalarTy PGText
        , "does the string exist as a top-level key in the column"
        )
      , ( "_has_keys_any"
        , G.toGT $ G.toLT $ G.toNT $ mkScalarTy PGText
        , "do any of these strings exist as top-level keys in the column"
        )
      , ( "_has_keys_all"
        , G.toGT $ G.toLT $ G.toNT $ mkScalarTy PGText
        , "do all of these strings exist as top-level keys in the column"
        )
      ]

    castOpInputValues =
      -- currently, only geometry/geography types support casting
      guard (isScalarWhere isGeoType) $>
        InpValInfo Nothing "_cast" Nothing (G.toGT $ mkCastExpTy colTy)

    stDWithinGeoOpInpVal ty =
      InpValInfo (Just stDWithinGeoDesc) "_st_d_within" Nothing $ G.toGT ty
    stDWithinGeoDesc =
      "is the column within a distance from a " <> colTyDesc <> " value"

    geoOpToInpVal (opName, desc) =
      InpValInfo (Just desc) opName Nothing $ G.toGT colGqlType

    -- operators applicable only to geometry types
    geomOps :: [(G.Name, G.Description)]
    geomOps =
      [
        ( "_st_contains"
        , "does the column contain the given geometry value"
        )
      , ( "_st_crosses"
        , "does the column crosses the given geometry value"
        )
      , ( "_st_equals"
        , "is the column equal to given geometry value. Directionality is ignored"
        )
      , ( "_st_overlaps"
        , "does the column 'spatially overlap' (intersect but not completely contain) the given geometry value"
        )
      , ( "_st_touches"
        , "does the column have atleast one point in common with the given geometry value"
        )
      , ( "_st_within"
        , "is the column contained in the given geometry value"
        )
      ]

    -- operators applicable to geometry and geography types
    geoOps =
      [
        ( "_st_intersects"
        , "does the column spatially intersect the given " <> colTyDesc <> " value"
        )
      ]

    -- raster related operators
    rasterOps =
      [
        ( "_st_intersects_rast"
        , G.toGT $ mkScalarTy PGRaster
        , boolFnMsg <> "ST_Intersects(raster <raster-col>, raster <raster-input>)"
        )
      , ( "_st_intersects_nband_geom"
        , G.toGT stIntersectsNbandGeomInputTy
        , boolFnMsg <> "ST_Intersects(raster <raster-col>, integer nband, geometry geommin)"
        )
      , ( "_st_intersects_geom_nband"
        , G.toGT stIntersectsGeomNbandInputTy
        , boolFnMsg <> "ST_Intersects(raster <raster-col> , geometry geommin, integer nband=NULL)"
        )
      ]

    boolFnMsg = "evaluates the following boolean Postgres function; "

geoInputTypes :: [InpObjTyInfo]
geoInputTypes =
  [ stDWithinGeometryInputType
  , stDWithinGeographyInputType
  , mkCastExpressionInputType (PGColumnScalar PGGeometry) [PGColumnScalar PGGeography]
  , mkCastExpressionInputType (PGColumnScalar PGGeography) [PGColumnScalar PGGeometry]
  ]
  where
    stDWithinGeometryInputType =
      mkHsraInpTyInfo Nothing stDWithinGeometryInpTy $ fromInpValL
      [ InpValInfo Nothing "from" Nothing $ G.toGT $ G.toNT $ mkScalarTy PGGeometry
      , InpValInfo Nothing "distance" Nothing $ G.toNT $ mkScalarTy PGFloat
      ]
    stDWithinGeographyInputType =
      mkHsraInpTyInfo Nothing stDWithinGeographyInpTy $ fromInpValL
      [ InpValInfo Nothing "from" Nothing $ G.toGT $ G.toNT $ mkScalarTy PGGeography
      , InpValInfo Nothing "distance" Nothing $ G.toNT $ mkScalarTy PGFloat
      , InpValInfo
        Nothing "use_spheroid" (Just $ G.VCBoolean True) $ G.toGT $ mkScalarTy PGBoolean
      ]

stIntersectsNbandGeomInputTy :: G.NamedType
stIntersectsNbandGeomInputTy = G.NamedType "st_intersects_nband_geom_input"

stIntersectsGeomNbandInputTy :: G.NamedType
stIntersectsGeomNbandInputTy = G.NamedType "st_intersects_geom_nband_input"

rasterIntersectsInputTypes :: [InpObjTyInfo]
rasterIntersectsInputTypes =
  [ stIntersectsNbandGeomInput
  , stIntersectsGeomNbandInput
  ]
  where
    stIntersectsNbandGeomInput =
      mkHsraInpTyInfo Nothing stIntersectsNbandGeomInputTy $ fromInpValL
      [ InpValInfo Nothing "nband" Nothing $
        G.toGT $ G.toNT $ mkScalarTy PGInteger
      , InpValInfo Nothing "geommin" Nothing $
        G.toGT $ G.toNT $ mkScalarTy PGGeometry
      ]

    stIntersectsGeomNbandInput =
      mkHsraInpTyInfo Nothing stIntersectsGeomNbandInputTy $ fromInpValL
      [ InpValInfo Nothing "geommin" Nothing $
        G.toGT $ G.toNT $ mkScalarTy PGGeometry
      , InpValInfo Nothing "nband" Nothing $
        G.toGT $ mkScalarTy PGInteger
      ]

mkBoolExpName :: QualifiedTable -> G.Name
mkBoolExpName tn =
  qualObjectToName tn <> "_bool_exp"

mkBoolExpTy :: QualifiedTable -> G.NamedType
mkBoolExpTy =
  G.NamedType . mkBoolExpName

-- table_bool_exp
mkBoolExpInp
  :: QualifiedTable
  -- the fields that are allowed
  -> [SelField]
  -> InpObjTyInfo
mkBoolExpInp tn fields =
  mkHsraInpTyInfo (Just desc) boolExpTy $ Map.fromList
    [(_iviName inpVal, inpVal) | inpVal <- inpValues]
  where
    desc = G.Description $
      "Boolean expression to filter rows from the table " <> tn <<>
      ". All fields are combined with a logical 'AND'."

    -- the type of this boolean expression
    boolExpTy = mkBoolExpTy tn

    -- all the fields of this input object
    inpValues = combinators <> mapMaybe mkFldExpInp fields

    mk n ty = InpValInfo Nothing n Nothing $ G.toGT ty

    boolExpListTy = G.toLT boolExpTy

    combinators =
      [ mk "_not" boolExpTy
      , mk "_and" boolExpListTy
      , mk "_or"  boolExpListTy
      ]

    mkFldExpInp = \case
      SFPGColumn (PGColumnInfo _ name _ colTy _ _) ->
        Just $ mk name (mkCompExpTy colTy)
      SFRelationship relationshipField ->
        let relationshipName = riName $ _rfiInfo relationshipField
            remoteTable = riRTable $  _rfiInfo relationshipField
        in Just $ mk (mkRelName relationshipName) (mkBoolExpTy remoteTable)
      SFComputedField _ -> Nothing -- TODO: support computed fields in bool exps
