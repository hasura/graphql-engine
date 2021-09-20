{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Roles.Internal
  ( CheckPermission (..)
  , CombineRolePermInfo (..)
  , rolePermInfoToCombineRolePermInfo
  , maybeToCheckPermission
  ) where

import           Hasura.Prelude

import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Semigroup                (Any (..), Max (..))

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCache
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend

-- | 'CheckPermission' is a type which can be used to combine multiple
--   permissions when the permission type implements the @OnlyRelevantEq@
--   instance
data CheckPermission permissionType
  = CPUndefined
  | CPInconsistent
  | CPDefined permissionType

deriving instance (Show permissionType) => Show (CheckPermission permissionType)
deriving instance (Eq permissionType) => Eq (CheckPermission permissionType)

instance (OnlyRelevantEq permissionType) =>
         Semigroup (CheckPermission permissionType) where
  CPUndefined <> a = a
  a <> CPUndefined = a
  CPInconsistent <> _ = CPInconsistent
  _ <> CPInconsistent = CPInconsistent
  CPDefined d1 <> CPDefined d2
    | d1 `relevantEq` d2 = CPDefined d1
    | otherwise = CPInconsistent

instance (OnlyRelevantEq permissionType) => Monoid (CheckPermission permissionType) where
  mempty = CPUndefined

-- | CombineRolePermInfo acts as an intermediate type to be able to
--   combine multiple role permissions into one, using the `Monoid`
--   instance. Multiple role permissions are combined for inherited
--   role permissions where this is used.
data CombineRolePermInfo (b :: BackendType)
  = CombineRolePermInfo
  { crpiInsPerm :: !(CheckPermission (InsPermInfo b))
  , crpiSelPerm :: !(Maybe (CombinedSelPermInfo b))
  , crpiUpdPerm :: !(CheckPermission (UpdPermInfo b))
  , crpiDelPerm :: !(CheckPermission (DelPermInfo b))
  }

instance ( Backend b
         , Eq (BooleanOperators b (PartialSQLExp b))
         , Hashable (BooleanOperators b (PartialSQLExp b))) => Semigroup (CombineRolePermInfo b) where
  CombineRolePermInfo insPermL selPermL updPermL delPermL <>
    CombineRolePermInfo insPermR selPermR updPermR delPermR =
    CombineRolePermInfo
      (insPermL <> insPermR)
      (selPermL <> selPermR)
      (updPermL <> updPermR)
      (delPermL <> delPermR)

instance ( Backend b
         , Eq (BooleanOperators b (PartialSQLExp b))
         , Hashable (BooleanOperators b (PartialSQLExp b))) => Monoid (CombineRolePermInfo b) where
  mempty = CombineRolePermInfo mempty mempty mempty mempty

rolePermInfoToCombineRolePermInfo :: RolePermInfo b -> CombineRolePermInfo b
rolePermInfoToCombineRolePermInfo RolePermInfo {..} =
  CombineRolePermInfo
    (maybeToCheckPermission _permIns)
    (modifySingleSelectPerm <$> _permSel)
    (maybeToCheckPermission _permUpd)
    (maybeToCheckPermission _permDel)
  where
    modifySingleSelectPerm SelPermInfo {..} =
      let columnCaseBoolExp = fmap AnnColumnCaseBoolExpField spiFilter
          colsWithColCaseBoolExp = spiCols $> Just columnCaseBoolExp
          scalarCompFieldsWithColCaseBoolExp = spiScalarComputedFields $> Just columnCaseBoolExp
      in
        CombinedSelPermInfo [colsWithColCaseBoolExp]
                            [scalarCompFieldsWithColCaseBoolExp]
                            [spiFilter]
                            (Max <$> spiLimit)
                            (Any spiAllowAgg)
                            spiRequiredHeaders

-- | `OnlyRelevantEq` is a type class to implement checking if
--   two types have the relevant info of a type equal. This typeclass is almost
--   like the `Eq` typeclass but gives the flexibility of having custom
--   rules to check the equality of only the relevant data. Some use cases of
--   it are:
--
--   1. When comparing two `G.ScalarTypeDefinition`, the description of the type
--      doesn't matter.
--   2. The directives are represented as a list, but the order of the directives don't matter
--
--   Note: When there's nothing there to discard in a type, the `OnlyRelevantEq` can be derived
--   as (if needed)
--
--   instance OnlyRelevantEq a where
--     relevantEq = (==)
--
--   NOTE: Do not export this type class from this module, as it is only intended to be used in
--   the context of inherited roles. If you think this typeclass fits your use case, then please
--   discuss this with Karthikeyan or in Slack thread to make this a generic typeclass.
class OnlyRelevantEq a where
  relevantEq :: a -> a -> Bool

instance (Backend b, Eq a, Hashable a) => OnlyRelevantEq (GBoolExp b a) where
  BoolAnd boolExpL    `relevantEq` BoolAnd boolExpR    = Set.fromList boolExpL == Set.fromList boolExpR
  BoolOr  boolExpL    `relevantEq` BoolOr  boolExpR    = Set.fromList boolExpL == Set.fromList boolExpR
  BoolNot boolExpL    `relevantEq` BoolNot boolExpR    = boolExpL == boolExpR
  BoolExists boolExpL `relevantEq` BoolExists boolExpR = boolExpL == boolExpR
  BoolFld boolExpL    `relevantEq` BoolFld    boolExpR = boolExpL == boolExpR
  _                   `relevantEq` _                   = False

instance (Backend b, Eq a, Eq (BooleanOperators b a)) => OnlyRelevantEq (AnnComputedFieldBoolExp b a) where
  relevantEq = (==)

instance (Backend b, Hashable a, Eq a, Hashable (BooleanOperators b a), Eq (BooleanOperators b a)) => OnlyRelevantEq (AnnBoolExpFld b a) where
  annBoolExpFldL `relevantEq` annBoolExpFldR =
    case (annBoolExpFldL, annBoolExpFldR) of
      (AVColumn colInfoL opExpsL, AVColumn colInfoR opExpsR) ->
        colInfoL == colInfoR && Set.fromList opExpsL == Set.fromList opExpsR
      (AVRelationship relInfoL annBoolExpL, AVRelationship relInfoR annBoolExpR) ->
        relInfoL == relInfoR && annBoolExpL `relevantEq` annBoolExpR
      (AVComputedField annCompFldBoolExpL, AVComputedField annCompFldBoolExpR) ->
        annCompFldBoolExpL `relevantEq` annCompFldBoolExpR
      (_, _) -> False


instance ( Backend b
         , Eq (BooleanOperators b (PartialSQLExp b))
         , Hashable (BooleanOperators b (PartialSQLExp b))
         ) => OnlyRelevantEq (InsPermInfo b) where
  (InsPermInfo colsL checkL setL backendOnlyL reqHeadersL) `relevantEq`
    (InsPermInfo colsR checkR setR backendOnlyR reqHeadersR) =
    colsL == colsR
    && checkL `relevantEq` checkR
    && setL == setR
    && backendOnlyL == backendOnlyR
    && reqHeadersL == reqHeadersR

instance ( Backend b
         , Eq (BooleanOperators b (PartialSQLExp b))
         , Hashable (BooleanOperators b (PartialSQLExp b))
         ) => OnlyRelevantEq (UpdPermInfo b) where
  (UpdPermInfo colsL tableL filterL checkL setL reqHeadersL)`relevantEq`
    (UpdPermInfo colsR tableR filterR checkR setR reqHeadersR) =
    colsL == colsR
    && tableL == tableR
    && filterL `relevantEq` filterR
    && checkL `relevantEq` checkR
    && setL == setR
    && reqHeadersL == reqHeadersR

instance ( Backend b
         , Eq (BooleanOperators b (PartialSQLExp b))
         , Hashable (BooleanOperators b (PartialSQLExp b))
         ) => OnlyRelevantEq (DelPermInfo b) where
  DelPermInfo tableL filterL reqHeadersL `relevantEq`
    DelPermInfo tableR filterR reqHeadersR =
    tableL == tableR
    && filterL `relevantEq` filterR
    && reqHeadersL == reqHeadersR

instance OnlyRelevantEq RemoteSchemaInputValueDefinition where
  RemoteSchemaInputValueDefinition defnL presetL
    `relevantEq` RemoteSchemaInputValueDefinition defnR presetR =
    defnL `relevantEq` defnR && presetL == presetR

instance OnlyRelevantEq RemoteSchemaIntrospection where
  RemoteSchemaIntrospection typeDefinitionsL
    `relevantEq` RemoteSchemaIntrospection typeDefinitionsR =
    (sort typeDefinitionsL) `relevantEq` (sort typeDefinitionsR)

instance OnlyRelevantEq IntrospectionResult where
  IntrospectionResult (RemoteSchemaIntrospection typeDefnsL) queryRootL mutationRootL subsRootL `relevantEq`
    IntrospectionResult (RemoteSchemaIntrospection typeDefnsR) queryRootR mutationRootR subsRootR =
    (sort typeDefnsL) `relevantEq` (sort typeDefnsR)
    && queryRootL == queryRootR
    && mutationRootL == mutationRootR
    && subsRootL == subsRootR

instance (OnlyRelevantEq a) => OnlyRelevantEq (Maybe a) where
  relevantEq l r =
    case (l, r) of
      (Just l', Just r') -> l' `relevantEq` r'
      (Nothing, Nothing) -> True
      _                  -> False

instance OnlyRelevantEq G.Name where
  relevantEq = (==)

instance OnlyRelevantEq a => OnlyRelevantEq [a] where
  l `relevantEq` r =
    (length r == length r)
    && (all (==True) (zipWith relevantEq l r))

instance OnlyRelevantEq G.ScalarTypeDefinition where
  G.ScalarTypeDefinition _descL nameL directivesL
    `relevantEq` G.ScalarTypeDefinition _descR nameR directivesR =
    nameL == nameR && Set.fromList directivesL == Set.fromList directivesR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.FieldDefinition a) where
  G.FieldDefinition _descL nameL argumentsL typeL directivesL
    `relevantEq` G.FieldDefinition _descR nameR argumentsR typeR directivesR =
    nameL == nameR
    && sort argumentsL `relevantEq` sort argumentsR
    && typeL == typeR
    && Set.fromList directivesL == Set.fromList directivesR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.ObjectTypeDefinition a) where
  G.ObjectTypeDefinition _descL nameL implementsInterfacesL directivesL fieldDefnsL
    `relevantEq` G.ObjectTypeDefinition _descR nameR implementsInterfacesR directivesR fieldDefnsR =
    nameL == nameR
    && Set.fromList implementsInterfacesL == Set.fromList implementsInterfacesR
    && Set.fromList directivesL == Set.fromList directivesR
    && sort fieldDefnsL `relevantEq` sort fieldDefnsR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.InterfaceTypeDefinition [G.Name] a) where
  G.InterfaceTypeDefinition _descL nameL directivesL fieldDefnsL possibleTypesL
    `relevantEq` G.InterfaceTypeDefinition _descR nameR directivesR fieldDefnsR possibleTypesR =
    nameL == nameR
    && Set.fromList directivesL == Set.fromList directivesR
    && sort fieldDefnsL `relevantEq` sort fieldDefnsR
    && Set.fromList possibleTypesL == Set.fromList possibleTypesR

instance OnlyRelevantEq G.UnionTypeDefinition where
  G.UnionTypeDefinition _descL nameL directivesL membersL
    `relevantEq` G.UnionTypeDefinition _descR nameR directivesR membersR =
    nameL == nameR
    && Set.fromList directivesL == Set.fromList directivesR
    && Set.fromList membersL == Set.fromList membersR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.InputObjectTypeDefinition a) where
  G.InputObjectTypeDefinition _descL nameL directivesL defnsL
    `relevantEq` G.InputObjectTypeDefinition _descR nameR directivesR defnsR =
    nameL == nameR
    && Set.fromList directivesL == Set.fromList directivesR
    && sort defnsL `relevantEq` sort defnsR

instance OnlyRelevantEq G.EnumValueDefinition where
  G.EnumValueDefinition _descL nameL directivesL
    `relevantEq` G.EnumValueDefinition _descR nameR directivesR =
    nameL == nameR && Set.fromList directivesL == Set.fromList directivesR

instance OnlyRelevantEq G.EnumTypeDefinition where
  G.EnumTypeDefinition _descL nameL directivesL valueDefnsL
    `relevantEq` G.EnumTypeDefinition _descR nameR directivesR valueDefnsR =
    nameL == nameR
    && Set.fromList directivesL == Set.fromList directivesR
    && sort valueDefnsL `relevantEq` sort valueDefnsR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.TypeDefinition [G.Name] a) where
  G.TypeDefinitionScalar scalarDefnL `relevantEq` G.TypeDefinitionScalar scalarDefnR = scalarDefnL `relevantEq` scalarDefnR
  G.TypeDefinitionObject objDefnL `relevantEq` G.TypeDefinitionObject objDefnR = objDefnL `relevantEq` objDefnR
  G.TypeDefinitionInterface interfaceDefnL `relevantEq` G.TypeDefinitionInterface interfaceDefnR = interfaceDefnL `relevantEq` interfaceDefnR
  G.TypeDefinitionUnion unionDefnL `relevantEq` G.TypeDefinitionUnion unionDefnR = unionDefnL `relevantEq` unionDefnR
  G.TypeDefinitionEnum enumDefnL `relevantEq` G.TypeDefinitionEnum enumDefnR = enumDefnL `relevantEq` enumDefnR
  G.TypeDefinitionInputObject inpObjDefnL `relevantEq` G.TypeDefinitionInputObject inpObjDefnR = inpObjDefnL `relevantEq` inpObjDefnR
  _ `relevantEq` _ = False

instance OnlyRelevantEq G.InputValueDefinition where
  G.InputValueDefinition _descL nameL typeL defaultValueL directivesL
    `relevantEq` G.InputValueDefinition _descR nameR typeR defaultValueR directivesR =
    nameL == nameR
    && typeL == typeR
    && defaultValueL == defaultValueR
    && Set.fromList directivesL == Set.fromList directivesR

maybeToCheckPermission :: Maybe a -> CheckPermission a
maybeToCheckPermission = maybe CPUndefined CPDefined
