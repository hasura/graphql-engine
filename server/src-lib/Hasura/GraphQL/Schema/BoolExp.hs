module Hasura.GraphQL.Schema.BoolExp
  ( geoInputTypes
  , mkCompExpInp

  , mkBoolExpTy
  , mkBoolExpInp
  ) where

import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

mkCompExpTy :: PGColType -> G.NamedType
mkCompExpTy =
  G.NamedType . mkCompExpName

mkCompExpName :: PGColType -> G.Name
mkCompExpName pgColTy =
  G.Name $ T.pack (show pgColTy) <> "_comparison_exp"

mkCastExpName :: PGColType -> G.Name
mkCastExpName pgColTy = G.Name $ T.pack (show pgColTy) <> "_cast_exp"

mkCastExpTy :: PGColType -> G.NamedType
mkCastExpTy = G.NamedType . mkCastExpName

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
mkCastExpressionInputType :: PGColType -> [PGColType] -> InpObjTyInfo
mkCastExpressionInputType sourceType targetTypes =
  mkHsraInpTyInfo (Just description) (mkCastExpTy sourceType) (fromInpValL targetFields)
  where
    description = mconcat
      [ "Expression to compare the result of casting a column of type "
      , G.Description (T.pack $ show sourceType)
      , ". Multiple cast targets are combined with logical 'AND'."
      ]
    targetFields = map targetField targetTypes
    targetField targetType = InpValInfo
      Nothing
      (G.Name . T.pack $ show targetType)
      Nothing
      (G.toGT $ mkCompExpTy targetType)

--- | make compare expression input type
mkCompExpInp :: PGColType -> InpObjTyInfo
mkCompExpInp colTy =
  InpObjTyInfo (Just tyDesc) (mkCompExpTy colTy) (fromInpValL $ concat
  [ map (mk colScalarTy) typedOps
  , map (mk $ G.toLT $ G.toNT colScalarTy) listOps
  , bool [] (map (mk $ mkScalarTy PGText) stringOps) isStringTy
  , bool [] (map jsonbOpToInpVal jsonbOps) isJsonbTy
  , bool [] (stDWithinGeoOpInpVal stDWithinGeometryInpTy :
             map geoOpToInpVal (geoOps ++ geomOps)) isGeometryType
  , bool [] (stDWithinGeoOpInpVal stDWithinGeographyInpTy :
             map geoOpToInpVal geoOps) isGeographyType
  , [InpValInfo Nothing "_is_null" Nothing $ G.TypeNamed (G.Nullability True) $ G.NamedType "Boolean"]
  , maybeToList castOpInputValue
  ]) TLHasuraType
  where
    tyDesc = mconcat
      [ "expression to compare columns of type "
      , G.Description (T.pack $ show colTy)
      , ". All fields are combined with logical 'AND'."
      ]
    isStringTy = case colTy of
      PGVarchar -> True
      PGText    -> True
      _         -> False
    mk t n = InpValInfo Nothing n Nothing $ G.toGT t
    colScalarTy = mkScalarTy colTy
    -- colScalarListTy = GA.GTList colGTy
    typedOps =
       ["_eq", "_neq", "_gt", "_lt", "_gte", "_lte"]
    listOps =
      [ "_in", "_nin" ]
    -- TODO
    -- columnOps =
    --   [ "_ceq", "_cneq", "_cgt", "_clt", "_cgte", "_clte"]
    stringOps =
      [ "_like", "_nlike", "_ilike", "_nilike"
      , "_similar", "_nsimilar"
      ]

    isJsonbTy = case colTy of
      PGJSONB -> True
      _       -> False
    jsonbOpToInpVal (op, ty, desc) = InpValInfo (Just desc) op Nothing ty
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

    castOpInputValue =
      -- currently, only geometry/geography types support casting
      guard (isGeoType colTy) $>
        InpValInfo Nothing "_cast" Nothing (G.toGT $ mkCastExpTy colTy)

    stDWithinGeoOpInpVal ty =
      InpValInfo (Just stDWithinGeoDesc) "_st_d_within" Nothing $ G.toGT ty
    stDWithinGeoDesc =
      "is the column within a distance from a " <> colTyDesc <> " value"

    -- Geometry related ops
    isGeometryType = case colTy of
      PGGeometry -> True
      _          -> False

    -- Geography related ops
    isGeographyType = case colTy of
      PGGeography -> True
      _           -> False

    geoOpToInpVal (op, desc) =
      InpValInfo (Just desc) op Nothing $ G.toGT $ mkScalarTy colTy

    colTyDesc = G.Description $ T.pack $ show colTy

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

geoInputTypes :: [InpObjTyInfo]
geoInputTypes =
  [ stDWithinGeometryInputType
  , stDWithinGeographyInputType
  , castGeometryInputType
  , castGeographyInputType
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

    castGeometryInputType = mkCastExpressionInputType PGGeometry [PGGeography]
    castGeographyInputType = mkCastExpressionInputType PGGeography [PGGeometry]

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
      SelFldCol (PGColInfo colName colTy _) -> Just $
        mk (mkColName colName) (mkCompExpTy colTy)
      SelFldRel (RelInfo relName _ _ remTab _, _, _, _, _) -> Just $
        mk (mkRelName relName) (mkBoolExpTy remTab)
      SelFldRemote {} -> Nothing
