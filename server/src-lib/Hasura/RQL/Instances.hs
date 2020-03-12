{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Instances where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import qualified Data.URL.Template             as UT
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Language.Haskell.TH.Syntax    as TH

import           Control.DeepSeq (NFData(..))
import           Instances.TH.Lift             ()

instance NFData G.FragmentDefinition
instance NFData G.GType
instance NFData G.OperationType
instance NFData G.VariableDefinition
instance NFData UT.Variable
instance NFData UT.TemplateItem
instance NFData UT.URLTemplate

instance NFData G.Name where
  rnf = rnf . G.unName

instance NFData a => NFData (G.Directive a)
instance NFData a => NFData (G.ExecutableDefinition a)
instance NFData a => NFData (G.Field a)
instance NFData a => NFData (G.FragmentSpread a)
instance NFData a => NFData (G.InlineFragment a)
instance NFData a => NFData (G.OperationDefinition a)
instance NFData a => NFData (G.Selection a)
instance NFData a => NFData (G.TypedOperationDefinition a)
instance NFData a => NFData (G.Value a)

deriving instance NFData G.EnumValue
deriving instance NFData G.Nullability

deriving instance NFData a => NFData (G.ExecutableDocument a)

instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [| M.fromList $(TH.lift $ M.toList m) |]

instance TH.Lift a => TH.Lift (S.HashSet a) where
  lift s = [| S.fromList $(TH.lift $ S.toList s) |]
