module Hasura.Backends.Postgres.Types.BoolExp where

import           Hasura.Prelude

import           Data.Aeson.Extended

import           Hasura.Incremental    (Cacheable)
import           Hasura.RQL.IR.BoolExp


data BooleanOperators a
  = AILIKE  !a --     ILIKE, case insensitive
  | ANILIKE !a -- NOT ILIKE, case insensitive

  | ASIMILAR  !a --     similar, regex
  | ANSIMILAR !a -- not similar, regex

  | AREGEX   !a -- regex: match POSIX case sensitive
  | AIREGEX  !a -- regex: match POSIX case insensitive
  | ANREGEX  !a -- regex: dont match POSIX case sensitive
  | ANIREGEX !a -- regex: dont match POSIX case insensitive

  | AContains    !a
  | AContainedIn !a
  | AHasKey      !a
  | AHasKeysAny  !a
  | AHasKeysAll  !a

  | ASTContains       !a
  | ASTCrosses        !a
  | ASTEquals         !a
  | ASTIntersects     !a
  | AST3DIntersects     !a
  | ASTOverlaps       !a
  | ASTTouches        !a
  | ASTWithin         !a
  | ASTIntersectsRast !a
  | ASTDWithinGeom         !(DWithinGeomOp a)
  | AST3DDWithinGeom       !(DWithinGeomOp a)
  | ASTDWithinGeog         !(DWithinGeogOp a)
  | ASTIntersectsGeomNband !(STIntersectsGeomminNband a)
  | ASTIntersectsNbandGeom !(STIntersectsNbandGeommin a)

  | AAncestor        !a
  | AAncestorAny     !a
  | ADescendant      !a
  | ADescendantAny   !a
  | AMatches         !a
  | AMatchesAny      !a
  | AMatchesFulltext !a
  deriving (Eq, Generic, Functor, Foldable, Traversable)

instance NFData    a => NFData   (BooleanOperators a)
instance Hashable  a => Hashable (BooleanOperators a)
instance Cacheable a => Cacheable (BooleanOperators a)


instance ToJSON a => ToJSONKeyValue (BooleanOperators a) where
  toJSONKeyValue = \case
    AILIKE  a                -> ("_ilike",                    toJSON a)
    ANILIKE a                -> ("_nilike",                   toJSON a)

    ASIMILAR  a              -> ("_similar",                  toJSON a)
    ANSIMILAR a              -> ("_nsimilar",                 toJSON a)

    AREGEX   a               -> ("_regex",                    toJSON a)
    AIREGEX  a               -> ("_iregex",                   toJSON a)
    ANREGEX  a               -> ("_nregex",                   toJSON a)
    ANIREGEX a               -> ("_niregex",                  toJSON a)

    AContains    a           -> ("_contains",                 toJSON a)
    AContainedIn a           -> ("_contained_in",             toJSON a)
    AHasKey      a           -> ("_has_key",                  toJSON a)
    AHasKeysAny  a           -> ("_has_keys_any",             toJSON a)
    AHasKeysAll  a           -> ("_has_keys_all",             toJSON a)

    ASTContains    a         -> ("_st_contains",              toJSON a)
    ASTCrosses     a         -> ("_st_crosses",               toJSON a)
    AST3DDWithinGeom o       -> ("_st_3d_d_within",           toJSON o)
    ASTDWithinGeom o         -> ("_st_d_within",              toJSON o)
    ASTDWithinGeog o         -> ("_st_d_within",              toJSON o)
    ASTEquals      a         -> ("_st_equals",                toJSON a)
    ASTIntersects  a         -> ("_st_intersects",            toJSON a)
    AST3DIntersects  a       -> ("_st_3d_intersects",         toJSON a)
    ASTOverlaps    a         -> ("_st_overlaps",              toJSON a)
    ASTTouches     a         -> ("_st_touches",               toJSON a)
    ASTWithin      a         -> ("_st_within",                toJSON a)

    ASTIntersectsRast      a -> ("_st_intersects_rast",       toJSON a)
    ASTIntersectsNbandGeom a -> ("_st_intersects_nband_geom", toJSON a)
    ASTIntersectsGeomNband a -> ("_st_intersects_geom_nband", toJSON a)

    AAncestor         a      -> ("_ancestor",                 toJSON a)
    AAncestorAny      a      -> ("_ancestor_any",             toJSON a)
    ADescendant       a      -> ("_descendant",               toJSON a)
    ADescendantAny    a      -> ("_descendant_any",           toJSON a)
    AMatches          a      -> ("_matches",                  toJSON a)
    AMatchesAny       a      -> ("_matches_any",              toJSON a)
    AMatchesFulltext  a      -> ("_matches_fulltext",         toJSON a)
