-- | Schema parsers for logical models
module Hasura.LogicalModel.Schema
  ( buildLogicalModelIR,
    buildLogicalModelPermissions,
    buildLogicalModelFields,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasura.GraphQL.Schema.Backend
  ( BackendLogicalModelSelectSchema (..),
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
  ( logicalModelSelectionList,
  )
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.IR (LogicalModel (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (gBoolExpTrue)
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelInfo)
import Hasura.RQL.Types.Roles (RoleName, adminRoleName)
import Hasura.Table.Cache (SelPermInfo (..), _permSel)

-- | find list of columns we're allowed to access for this role
getSelPermInfoForLogicalModel ::
  RoleName ->
  LogicalModelInfo b ->
  Maybe (SelPermInfo b)
getSelPermInfoForLogicalModel role logicalModel =
  HashMap.lookup role (_lmiPermissions logicalModel) >>= _permSel

-- | build select permissions for logical model
-- `admin` can always select everything
logicalModelPermissions ::
  (Backend b) =>
  LogicalModelInfo b ->
  RoleName ->
  IR.TablePermG b (IR.UnpreparedValue b)
logicalModelPermissions logicalModel roleName = do
  if roleName == adminRoleName
    then IR.TablePerm gBoolExpTrue Nothing
    else case getSelPermInfoForLogicalModel roleName logicalModel of
      Just selectPermissions ->
        IR.TablePerm
          { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
            IR._tpLimit = spiLimit selectPermissions
          }
      Nothing -> IR.TablePerm gBoolExpTrue Nothing

-- | turn post-schema cache LogicalModelInfo into IR
buildLogicalModelIR :: LogicalModelInfo b -> LogicalModel b
buildLogicalModelIR LogicalModelInfo {..} =
  LogicalModel
    { lmName = _lmiName,
      lmFields = _lmiFields
    }

-- | top-level select permissions for a logical model
buildLogicalModelPermissions ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (IR.TablePermG b (IR.UnpreparedValue b))
buildLogicalModelPermissions logicalModel = do
  roleName <- retrieve scRole

  pure $ logicalModelPermissions logicalModel roleName

-- in order to construct meaningful IR, we can only parse a Logical Model
-- in the context of it's use, therefore we must pass in any information on
-- relationships (and then,
buildLogicalModelFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendLogicalModelSelectSchema b
  ) =>
  InsOrdHashMap RelName (RelInfo b) ->
  LogicalModelInfo b ->
  SchemaT
    r
    m
    ( Maybe
        ( P.Parser 'P.Output n (AnnotatedFields b),
          P.InputFieldsParser n (IR.SelectArgsG b (IR.UnpreparedValue b))
        )
    )
buildLogicalModelFields relationshipInfo logicalModel = runMaybeT $ do
  selectionSetParser <- MaybeT $ logicalModelSelectionList @b @r @m @n relationshipInfo logicalModel
  logicalModelsArgsParser <- lift $ logicalModelArguments @b @r @m @n logicalModel

  pure (selectionSetParser, logicalModelsArgsParser)
