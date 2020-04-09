{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Instances where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import qualified Data.URL.Template             as UT
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Language.Haskell.TH.Syntax    as TH
import qualified Text.Regex.TDFA               as TDFA
import qualified Text.Regex.TDFA.Pattern       as TDFA

import           Data.Functor.Product
import           Data.GADT.Compare
import           Instances.TH.Lift             ()

instance NFData G.Argument
instance NFData G.Directive
instance NFData G.ExecutableDefinition
instance NFData G.Field
instance NFData G.FragmentDefinition
instance NFData G.FragmentSpread
instance NFData G.GType
instance NFData G.InlineFragment
instance NFData G.OperationDefinition
instance NFData G.OperationType
instance NFData G.Selection
instance NFData G.TypedOperationDefinition
instance NFData G.Value
instance NFData G.ValueConst
instance NFData G.VariableDefinition
instance (NFData a) => NFData (G.ObjectFieldG a)
instance NFData UT.Variable
instance NFData UT.TemplateItem
instance NFData UT.URLTemplate

deriving instance NFData G.Alias
deriving instance NFData G.EnumValue
deriving instance NFData G.ExecutableDocument
deriving instance NFData G.ListType
deriving instance NFData G.Name
deriving instance NFData G.NamedType
deriving instance NFData G.Nullability
deriving instance NFData G.StringValue
deriving instance NFData G.Variable
deriving instance NFData G.Description
deriving instance (NFData a) => NFData (G.ListValueG a)
deriving instance (NFData a) => NFData (G.ObjectValueG a)

instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [| M.fromList $(TH.lift $ M.toList m) |]

instance TH.Lift a => TH.Lift (S.HashSet a) where
  lift s = [| S.fromList $(TH.lift $ S.toList s) |]

deriving instance TH.Lift TDFA.CompOption
deriving instance TH.Lift TDFA.DoPa
deriving instance TH.Lift TDFA.ExecOption
deriving instance TH.Lift TDFA.Pattern
deriving instance TH.Lift TDFA.PatternSet
deriving instance TH.Lift TDFA.PatternSetCharacterClass
deriving instance TH.Lift TDFA.PatternSetCollatingElement
deriving instance TH.Lift TDFA.PatternSetEquivalenceClass

instance (GEq f, GEq g) => GEq (Product f g) where
  Pair a1 a2 `geq` Pair b1 b2
    | Just Refl <- a1 `geq` b1
    , Just Refl <- a2 `geq` b2
    = Just Refl
    | otherwise = Nothing

instance (GCompare f, GCompare g) => GCompare (Product f g) where
  Pair a1 a2 `gcompare` Pair b1 b2 = case gcompare a1 b1 of
    GLT -> GLT
    GEQ -> case gcompare a2 b2 of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT
