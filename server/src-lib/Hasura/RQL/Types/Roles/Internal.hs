{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Roles.Internal
  ( CheckPermission (..),
    CombineRolePermInfo (..),
    rolePermInfoToCombineRolePermInfo,
    maybeToCheckPermission,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Semigroup (Any (..), Max (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.SchemaCache
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | 'CheckPermission' is a type which can be used to combine multiple
--   permissions when the permission type implements the @OnlyRelevantEq@
--   instance
data CheckPermission permissionType
  = CPUndefined
  | CPInconsistent
  | CPDefined permissionType
  deriving stock (Show, Eq)

instance
  (OnlyRelevantEq permissionType) =>
  Semigroup (CheckPermission permissionType)
  where
  CPUndefined <> a = a
  a <> CPUndefined = a
  CPInconsistent <> _ = CPInconsistent
  _ <> CPInconsistent = CPInconsistent
  CPDefined d1 <> CPDefined d2
    | d1 ==~ d2 = CPDefined d1
    | otherwise = CPInconsistent

instance
  (OnlyRelevantEq permissionType) =>
  Monoid (CheckPermission permissionType)
  where
  mempty = CPUndefined

-- | CombineRolePermInfo acts as an intermediate type to be able to
--   combine multiple role permissions into one, using the `Monoid`
--   instance. Multiple role permissions are combined for inherited
--   role permissions where this is used.
data CombineRolePermInfo (b :: BackendType) = CombineRolePermInfo
  { crpiInsPerm :: CheckPermission (InsPermInfo b),
    crpiSelPerm :: Maybe (CombinedSelPermInfo b),
    crpiUpdPerm :: CheckPermission (UpdPermInfo b),
    crpiDelPerm :: CheckPermission (DelPermInfo b)
  }

instance
  ( Backend b,
    Semigroup (CheckPermission (DelPermInfo b)),
    Semigroup (CheckPermission (InsPermInfo b)),
    Semigroup (CheckPermission (UpdPermInfo b)),
    Semigroup (CombinedSelPermInfo b)
  ) =>
  Semigroup (CombineRolePermInfo b)
  where
  CombineRolePermInfo insPermL selPermL updPermL delPermL
    <> CombineRolePermInfo insPermR selPermR updPermR delPermR =
      CombineRolePermInfo
        (insPermL <> insPermR)
        (selPermL <> selPermR)
        (updPermL <> updPermR)
        (delPermL <> delPermR)

instance
  ( Backend b,
    Monoid (CheckPermission (DelPermInfo b)),
    Monoid (CheckPermission (InsPermInfo b)),
    Monoid (CheckPermission (UpdPermInfo b)),
    Monoid (Maybe (CombinedSelPermInfo b))
  ) =>
  Monoid (CombineRolePermInfo b)
  where
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
      let colsWithRedactionExp = spiCols $> RedactIfFalse spiFilter
          scalarCompFieldsWithRedactionExp = spiComputedFields $> RedactIfFalse spiFilter
       in CombinedSelPermInfo
            [colsWithRedactionExp]
            [scalarCompFieldsWithRedactionExp]
            [spiFilter]
            (Max <$> spiLimit)
            (Any spiAllowAgg)
            spiRequiredHeaders
            spiAllowedQueryRootFields
            spiAllowedSubscriptionRootFields

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
--     (==~) = (==)
--
--   NOTE: Do not export this type class from this module, as it is only intended to be used in
--   the context of inherited roles. If you think this typeclass fits your use case, then please
--   discuss this with Karthikeyan or in Slack thread to make this a generic typeclass.
class OnlyRelevantEq a where
  infix 4 ==~ -- same as (==)
  (==~) :: a -> a -> Bool

instance (Backend b, Hashable a) => OnlyRelevantEq (GBoolExp b a) where
  BoolAnd boolExpL ==~ BoolAnd boolExpR = Set.fromList boolExpL == Set.fromList boolExpR
  BoolOr boolExpL ==~ BoolOr boolExpR = Set.fromList boolExpL == Set.fromList boolExpR
  BoolNot boolExpL ==~ BoolNot boolExpR = boolExpL == boolExpR
  BoolExists boolExpL ==~ BoolExists boolExpR = boolExpL == boolExpR
  BoolField boolExpL ==~ BoolField boolExpR = boolExpL == boolExpR
  _ ==~ _ = False

instance
  ( Eq (AnnComputedFieldBoolExp b a)
  ) =>
  OnlyRelevantEq (AnnComputedFieldBoolExp b a)
  where
  (==~) = (==)

instance
  ( Backend b,
    Hashable (OpExpG b a),
    OnlyRelevantEq (AnnBoolExp b a),
    OnlyRelevantEq (AnnComputedFieldBoolExp b a)
  ) =>
  OnlyRelevantEq (AnnBoolExpFld b a)
  where
  annBoolExpFldL ==~ annBoolExpFldR =
    case (annBoolExpFldL, annBoolExpFldR) of
      (AVColumn colInfoL redactionExpL opExpsL, AVColumn colInfoR redactionExpR opExpsR) ->
        colInfoL == colInfoR && Set.fromList opExpsL == Set.fromList opExpsR && redactionExpL ==~ redactionExpR
      (AVRelationship relInfoL (RelationshipFilters permsL annBoolExpL), AVRelationship relInfoR (RelationshipFilters permsR annBoolExpR)) ->
        relInfoL == relInfoR && annBoolExpL ==~ annBoolExpR && permsL ==~ permsR
      (AVComputedField annCompFldBoolExpL, AVComputedField annCompFldBoolExpR) ->
        annCompFldBoolExpL ==~ annCompFldBoolExpR
      (_, _) -> False

instance
  ( Backend b,
    OnlyRelevantEq (AnnBoolExpPartialSQL b)
  ) =>
  OnlyRelevantEq (InsPermInfo b)
  where
  (InsPermInfo colsL checkL setL backendOnlyL reqHeadersL validateInputL)
    ==~ (InsPermInfo colsR checkR setR backendOnlyR reqHeadersR validateInputR) =
      colsL
        == colsR
        && checkL
        ==~ checkR
        && setL
        == setR
        && backendOnlyL
        == backendOnlyR
        && reqHeadersL
        == reqHeadersR
        && validateInputL
        == validateInputR

instance
  ( Backend b,
    OnlyRelevantEq (AnnBoolExpPartialSQL b)
  ) =>
  OnlyRelevantEq (UpdPermInfo b)
  where
  (UpdPermInfo colsL tableL filterL checkL setL backendOnlyL reqHeadersL validateInputL)
    ==~ (UpdPermInfo colsR tableR filterR checkR setR backendOnlyR reqHeadersR validateInputR) =
      colsL
        == colsR
        && tableL
        == tableR
        && filterL
        ==~ filterR
        && checkL
        ==~ checkR
        && setL
        == setR
        && backendOnlyL
        == backendOnlyR
        && reqHeadersL
        == reqHeadersR
        && validateInputL
        == validateInputR

instance
  ( Backend b,
    OnlyRelevantEq (AnnBoolExpPartialSQL b)
  ) =>
  OnlyRelevantEq (DelPermInfo b)
  where
  (DelPermInfo tableL filterL backendOnlyL reqHeadersL validateInputL)
    ==~ (DelPermInfo tableR filterR backendOnlyR reqHeadersR validateInputR) =
      tableL
        == tableR
        && filterL
        ==~ filterR
        && backendOnlyL
        == backendOnlyR
        && reqHeadersL
        == reqHeadersR
        && validateInputL
        == validateInputR

instance OnlyRelevantEq RemoteSchemaInputValueDefinition where
  RemoteSchemaInputValueDefinition defnL presetL
    ==~ RemoteSchemaInputValueDefinition defnR presetR =
      defnL ==~ defnR && presetL == presetR

instance OnlyRelevantEq RemoteSchemaIntrospection where
  RemoteSchemaIntrospection typeDefinitionsL
    ==~ RemoteSchemaIntrospection typeDefinitionsR =
      sort (HashMap.elems typeDefinitionsL) ==~ sort (HashMap.elems typeDefinitionsR)

instance OnlyRelevantEq IntrospectionResult where
  IntrospectionResult (RemoteSchemaIntrospection typeDefnsL) queryRootL mutationRootL subsRootL
    ==~ IntrospectionResult (RemoteSchemaIntrospection typeDefnsR) queryRootR mutationRootR subsRootR =
      sort (HashMap.elems typeDefnsL)
        ==~ sort (HashMap.elems typeDefnsR)
        && queryRootL
        == queryRootR
        && mutationRootL
        == mutationRootR
        && subsRootL
        == subsRootR

instance (OnlyRelevantEq a) => OnlyRelevantEq (Maybe a) where
  (==~) l r =
    case (l, r) of
      (Just l', Just r') -> l' ==~ r'
      (Nothing, Nothing) -> True
      _ -> False

instance OnlyRelevantEq G.Name where
  (==~) = (==)

instance (OnlyRelevantEq a) => OnlyRelevantEq [a] where
  l ==~ r =
    (length r == length r)
      && (all (== True) (zipWith (==~) l r))

instance OnlyRelevantEq G.ScalarTypeDefinition where
  G.ScalarTypeDefinition _descL nameL directivesL
    ==~ G.ScalarTypeDefinition _descR nameR directivesR =
      nameL == nameR && Set.fromList directivesL == Set.fromList directivesR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.FieldDefinition a) where
  G.FieldDefinition _descL nameL argumentsL typeL directivesL
    ==~ G.FieldDefinition _descR nameR argumentsR typeR directivesR =
      nameL
        == nameR
        && sort argumentsL
        ==~ sort argumentsR
        && typeL
        == typeR
        && Set.fromList directivesL
        == Set.fromList directivesR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.ObjectTypeDefinition a) where
  G.ObjectTypeDefinition _descL nameL implementsInterfacesL directivesL fieldDefnsL
    ==~ G.ObjectTypeDefinition _descR nameR implementsInterfacesR directivesR fieldDefnsR =
      nameL
        == nameR
        && Set.fromList implementsInterfacesL
        == Set.fromList implementsInterfacesR
        && Set.fromList directivesL
        == Set.fromList directivesR
        && sort fieldDefnsL
        ==~ sort fieldDefnsR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.InterfaceTypeDefinition [G.Name] a) where
  G.InterfaceTypeDefinition _descL nameL directivesL fieldDefnsL possibleTypesL
    ==~ G.InterfaceTypeDefinition _descR nameR directivesR fieldDefnsR possibleTypesR =
      nameL
        == nameR
        && Set.fromList directivesL
        == Set.fromList directivesR
        && sort fieldDefnsL
        ==~ sort fieldDefnsR
        && Set.fromList possibleTypesL
        == Set.fromList possibleTypesR

instance OnlyRelevantEq G.UnionTypeDefinition where
  G.UnionTypeDefinition _descL nameL directivesL membersL
    ==~ G.UnionTypeDefinition _descR nameR directivesR membersR =
      nameL
        == nameR
        && Set.fromList directivesL
        == Set.fromList directivesR
        && Set.fromList membersL
        == Set.fromList membersR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.InputObjectTypeDefinition a) where
  G.InputObjectTypeDefinition _descL nameL directivesL defnsL
    ==~ G.InputObjectTypeDefinition _descR nameR directivesR defnsR =
      nameL
        == nameR
        && Set.fromList directivesL
        == Set.fromList directivesR
        && sort defnsL
        ==~ sort defnsR

instance OnlyRelevantEq G.EnumValueDefinition where
  G.EnumValueDefinition _descL nameL directivesL
    ==~ G.EnumValueDefinition _descR nameR directivesR =
      nameL == nameR && Set.fromList directivesL == Set.fromList directivesR

instance OnlyRelevantEq G.EnumTypeDefinition where
  G.EnumTypeDefinition _descL nameL directivesL valueDefnsL
    ==~ G.EnumTypeDefinition _descR nameR directivesR valueDefnsR =
      nameL
        == nameR
        && Set.fromList directivesL
        == Set.fromList directivesR
        && sort valueDefnsL
        ==~ sort valueDefnsR

instance (OnlyRelevantEq a, Ord a) => OnlyRelevantEq (G.TypeDefinition [G.Name] a) where
  G.TypeDefinitionScalar scalarDefnL ==~ G.TypeDefinitionScalar scalarDefnR = scalarDefnL ==~ scalarDefnR
  G.TypeDefinitionObject objDefnL ==~ G.TypeDefinitionObject objDefnR = objDefnL ==~ objDefnR
  G.TypeDefinitionInterface interfaceDefnL ==~ G.TypeDefinitionInterface interfaceDefnR = interfaceDefnL ==~ interfaceDefnR
  G.TypeDefinitionUnion unionDefnL ==~ G.TypeDefinitionUnion unionDefnR = unionDefnL ==~ unionDefnR
  G.TypeDefinitionEnum enumDefnL ==~ G.TypeDefinitionEnum enumDefnR = enumDefnL ==~ enumDefnR
  G.TypeDefinitionInputObject inpObjDefnL ==~ G.TypeDefinitionInputObject inpObjDefnR = inpObjDefnL ==~ inpObjDefnR
  _ ==~ _ = False

instance OnlyRelevantEq G.InputValueDefinition where
  G.InputValueDefinition _descL nameL typeL defaultValueL directivesL
    ==~ G.InputValueDefinition _descR nameR typeR defaultValueR directivesR =
      nameL
        == nameR
        && typeL
        == typeR
        && defaultValueL
        == defaultValueR
        && Set.fromList directivesL
        == Set.fromList directivesR

instance
  (OnlyRelevantEq (GBoolExp b (AnnBoolExpFld b v))) =>
  OnlyRelevantEq (AnnRedactionExp b v)
  where
  NoRedaction ==~ NoRedaction = True
  NoRedaction ==~ RedactIfFalse {} = False
  RedactIfFalse {} ==~ NoRedaction = False
  RedactIfFalse bExpL ==~ RedactIfFalse bExpR = bExpL ==~ bExpR

maybeToCheckPermission :: Maybe a -> CheckPermission a
maybeToCheckPermission = maybe CPUndefined CPDefined
