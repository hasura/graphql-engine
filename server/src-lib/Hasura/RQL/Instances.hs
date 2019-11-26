{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Instances where

import           Hasura.Prelude

import           Instances.TH.Lift             ()
import qualified Language.Haskell.TH.Syntax    as TH

import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import qualified Language.GraphQL.Draft.Syntax as G

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

deriving instance NFData G.Alias
deriving instance NFData G.EnumValue
deriving instance NFData G.ExecutableDocument
deriving instance NFData G.ListType
deriving instance NFData G.Name
deriving instance NFData G.NamedType
deriving instance NFData G.Nullability
deriving instance NFData G.StringValue
deriving instance NFData G.Variable
deriving instance (NFData a) => NFData (G.ListValueG a)
deriving instance (NFData a) => NFData (G.ObjectValueG a)

instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [| M.fromList $(TH.lift $ M.toList m) |]

instance TH.Lift a => TH.Lift (S.HashSet a) where
  lift s = [| S.fromList $(TH.lift $ S.toList s) |]
