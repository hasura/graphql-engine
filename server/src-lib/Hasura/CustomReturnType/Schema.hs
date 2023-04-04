-- | Schema parsers for custom return types
module Hasura.CustomReturnType.Schema
  ( buildCustomReturnTypeIR,
    buildCustomReturnTypePermissions,
    buildCustomReturnTypeFields,
  )
where

import Data.HashMap.Strict qualified as HM
import Hasura.CustomReturnType.Cache (CustomReturnTypeInfo (..))
import Hasura.CustomReturnType.IR (CustomReturnType (..))
import Hasura.CustomReturnType.Types (CustomReturnTypeName (..))
import Hasura.GraphQL.Schema.Backend
  ( BackendCustomReturnTypeSelectSchema (..),
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Common
  ( AnnotatedFields,
    SchemaT,
    partialSQLExpToUnpreparedValue,
    retrieve,
    scRole,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
  ( customReturnTypeSelectionList,
  )
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (gBoolExpTrue)
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Table (SelPermInfo (..), _permSel)
import Hasura.Session (RoleName, adminRoleName)

-- | find list of columns we're allowed to access for this role
getSelPermInfoForCustomReturnType ::
  RoleName ->
  CustomReturnTypeInfo b ->
  Maybe (SelPermInfo b)
getSelPermInfoForCustomReturnType role customReturnType =
  HM.lookup role (_crtiPermissions customReturnType) >>= _permSel

-- | build select permissions for custom return type
-- `admin` can always select everything
customReturnTypePermissions ::
  (Backend b) =>
  CustomReturnTypeInfo b ->
  RoleName ->
  IR.TablePermG b (IR.UnpreparedValue b)
customReturnTypePermissions customReturnType roleName = do
  if roleName == adminRoleName
    then IR.TablePerm gBoolExpTrue Nothing
    else case getSelPermInfoForCustomReturnType roleName customReturnType of
      Just selectPermissions ->
        IR.TablePerm
          { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
            IR._tpLimit = spiLimit selectPermissions
          }
      Nothing -> IR.TablePerm gBoolExpTrue Nothing

-- | turn post-schema cache CustomReturnTypeInfo into IR
buildCustomReturnTypeIR :: CustomReturnTypeInfo b -> CustomReturnType b
buildCustomReturnTypeIR CustomReturnTypeInfo {..} =
  CustomReturnType
    { crtName = _crtiName,
      crtFields = _crtiFields
    }

-- | top-level select permissions for a custom return type
buildCustomReturnTypePermissions ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  CustomReturnTypeInfo b ->
  SchemaT r m (IR.TablePermG b (IR.UnpreparedValue b))
buildCustomReturnTypePermissions customReturnType = do
  roleName <- retrieve scRole

  pure $ customReturnTypePermissions customReturnType roleName

buildCustomReturnTypeFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendCustomReturnTypeSelectSchema b
  ) =>
  CustomReturnTypeInfo b ->
  SchemaT
    r
    m
    ( Maybe
        ( P.Parser 'P.Output n (AnnotatedFields b),
          P.InputFieldsParser n (IR.SelectArgsG b (IR.UnpreparedValue b))
        )
    )
buildCustomReturnTypeFields customReturnType = runMaybeT $ do
  let fieldName = getCustomReturnTypeName (_crtiName customReturnType)
  selectionSetParser <- MaybeT $ customReturnTypeSelectionList @b @r @m @n fieldName customReturnType
  customTypesArgsParser <- lift $ customReturnTypeArguments @b @r @m @n fieldName customReturnType

  pure (selectionSetParser, customTypesArgsParser)
