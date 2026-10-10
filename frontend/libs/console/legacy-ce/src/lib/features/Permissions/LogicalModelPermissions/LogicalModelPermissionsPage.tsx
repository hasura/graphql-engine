import { LogicalModelPermissions } from './LogicalModelPermissions';
import { useCreateLogicalModelsPermissions } from './hooks/useCreateLogicalModelsPermissions';
import { useRemoveLogicalModelsPermissions } from './hooks/useRemoveLogicalModelsPermissions';
import { useMetadata } from '@hasura/metadata/api';
import { usePermissionComparators } from '../PermissionsForm/components/RowPermissionsBuilder/hooks/usePermissionComparators';
import { Flex, Skeleton } from '@radix-ui/themes';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { Text } from '@hasura/shared/ui';

export const LogicalModelPermissionsPage = ({
  source,
  name,
}: {
  source: string;
  name: string;
}) => {
  const { data, isLoading } = useMetadata((m) =>
    MetadataSelectors.extractModelsAndQueriesFromMetadata(m),
  );
  const comparators = usePermissionComparators();
  const { data: roles = [] } = useMetadata(MetadataSelectors.selectRoles);
  const logicalModels = data?.models ?? [];
  const logicalModel = logicalModels.find(
    (model) => model.name === name && model.source.name === source,
  );
  const { create, isPending: isCreating } = useCreateLogicalModelsPermissions({
    logicalModels,
    source: logicalModel?.source,
  });
  const { remove, isPending: isRemoving } = useRemoveLogicalModelsPermissions({
    logicalModels,
    source: logicalModel?.source,
  });
  return (
    <div
      className="mt-4"
      // Recreate the key when the logical model permissions change to reset the form
      key={logicalModel?.select_permissions?.length}
    >
      {isLoading ? (
        <Flex
          align="center"
          justify="center"
          className="h-64"
          data-testid="loading-logical-model-permissions"
        >
          <Skeleton height="224px" />
        </Flex>
      ) : !logicalModel ? (
        <Flex align="center" justify="center" className="h-64">
          <Text>
            Logical model with name {name} and driver {source} not found
          </Text>
        </Flex>
      ) : (
        <LogicalModelPermissions
          onSave={async (permission) => {
            create({
              logicalModelName: logicalModel?.name,
              permission,
              onSuccess: null,
            });
          }}
          onDelete={async (permission) => {
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
