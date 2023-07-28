import { InjectedRouter, withRouter } from 'react-router';
import { LogicalModelPermissions } from './LogicalModelPermissions';
import { useCreateLogicalModelsPermissions } from './hooks/useCreateLogicalModelsPermissions';
import { useRemoveLogicalModelsPermissions } from './hooks/useRemoveLogicalModelsPermissions';
import { useMetadata } from '../../hasura-metadata-api';
import { usePermissionComparators } from '../PermissionsForm/components/RowPermissionsBuilder/hooks/usePermissionComparators';
import Skeleton from 'react-loading-skeleton';
import { extractModelsAndQueriesFromMetadata } from '../../hasura-metadata-api/selectors';
import { LogicalModelTabs } from '../../Data/LogicalModels/components/LogicalModelTabs';
import { RouteWrapper } from '../../Data/LogicalModels/components/RouteWrapper';

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
        />
      )}
    </div>
  );
};

export const LogicalModelPermissionsRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
  params: {
    source: string;
    name: string;
  };
}>(({ params }) => {
  return (
    <RouteWrapper
      route={
        '/data/native-queries/logical-models/{{source}}/{{name}}/permissions'
      }
      itemSourceName={params.source}
      itemName={params.name}
    >
      <LogicalModelTabs
        source={params.source}
        name={params.name}
        defaultValue={'logical-model-permissions'}
      />
    </RouteWrapper>
  );
});
