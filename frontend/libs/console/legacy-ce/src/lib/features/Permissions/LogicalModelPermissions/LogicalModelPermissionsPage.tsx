import Skeleton from 'react-loading-skeleton';

import { LogicalModelPermissions } from './LogicalModelPermissions';
import { useCreateLogicalModelsPermissions } from './hooks/useCreateLogicalModelsPermissions';
import { useRemoveLogicalModelsPermissions } from './hooks/useRemoveLogicalModelsPermissions';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { extractModelsAndQueriesFromMetadata } from '../../hasura-metadata-api/selectors';
import { usePermissionComparators } from '../PermissionsForm/components/RowPermissionsBuilder/hooks/usePermissionComparators';

export const LogicalModelPermissionsPage = ({
  source,
  name,
}: {
  source: string;
  name: string;
}) => {
  const { data, isLoading } = useMetadata(m =>
    extractModelsAndQueriesFromMetadata(m)
  );
  const comparators = usePermissionComparators();
  const { data: roles = [] } = useMetadata(MetadataSelectors.getRoles);
  const logicalModels = data?.models ?? [];
  const logicalModel = logicalModels.find(
    model => model.name === name && model.source.name === source
  );
  const { create, isLoading: isCreating } = useCreateLogicalModelsPermissions({
    logicalModels,
    source: logicalModel?.source,
  });
  const { remove, isLoading: isRemoving } = useRemoveLogicalModelsPermissions({
    logicalModels,
    source: logicalModel?.source,
  });
  return (
    <div
      className="mt-md"
      // Recreate the key when the logical model permissions change to reset the form
      key={logicalModel?.select_permissions?.length}
    >
      {isLoading ? (
        <div
          className="flex items-center justify-center h-64"
          data-testid="loading-logical-model-permissions"
        >
          <Skeleton />
        </div>
      ) : !logicalModel ? (
        <div className="flex items-center justify-center h-64">
          <span className="text-gray-500">
            Logical model with name {name} and driver {source} not found
          </span>
        </div>
      ) : (
        <LogicalModelPermissions
          onSave={async permission => {
            create({
              logicalModelName: logicalModel?.name,
              permission,
            });
          }}
          onDelete={async permission => {
            remove({
              logicalModelName: logicalModel?.name,
              permission,
            });
          }}
          isCreating={isCreating}
          isRemoving={isRemoving}
          comparators={comparators}
          logicalModelName={logicalModel?.name}
          logicalModels={logicalModels}
          roles={roles}
        />
      )}
    </div>
  );
};
