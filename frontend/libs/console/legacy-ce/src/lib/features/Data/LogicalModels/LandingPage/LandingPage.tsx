import { Breadcrumbs } from '../../../../new-components/Breadcrumbs';
import { Tabs } from '../../../../new-components/Tabs';
import { useMetadata } from '../../../hasura-metadata-api';
import { ListLogicalModels } from './components/ListLogicalModels';
import { ListNativeQueries } from './components/ListNativeQueries';
import { extractModelsAndQueriesFromMetadata } from '../utils';
import {
  useDestructiveAlert,
  useHasuraAlert,
} from '../../../../new-components/Alert';
import { LogicalModelWithSource, NativeQueryWithSource } from '../types';
import { useTrackNativeQuery } from '../../hooks/useTrackNativeQuery';
import { useTrackLogicalModel } from '../../hooks/useTrackLogicalModel';
import { hasuraToast } from '../../../../new-components/Toasts';

export const LandingPage = () => {
  const { data, isLoading } = useMetadata(m =>
    extractModelsAndQueriesFromMetadata(m)
  );
  const nativeQueries = data?.queries ?? [];
  const logicalModels = data?.models ?? [];

  const { hasuraAlert } = useHasuraAlert();

  const { destructiveConfirm } = useDestructiveAlert();

  const { untrackNativeQuery } = useTrackNativeQuery();
  const { untrackLogicalModel } = useTrackLogicalModel();

  const handleRemoveNativeQuery = (q: NativeQueryWithSource) => {
    destructiveConfirm({
      resourceName: q.root_field_name,
      resourceType: 'Native Query',
      destroyTerm: 'remove',
      onConfirm: () =>
        new Promise(resolve => {
          untrackNativeQuery({
            data: { root_field_name: q.root_field_name, source: q.source },
            onSuccess: () => {
              resolve(true);
            },
            onError: err => {
              hasuraToast({
                type: 'error',
                title: 'Error',
                message: err.message,
              });
              resolve(false);
            },
          });
        }),
    });
  };
  const handleRemoveLogicalModel = (m: LogicalModelWithSource) => {
    destructiveConfirm({
      resourceName: m.name,
      resourceType: 'Logical Model',
      destroyTerm: 'remove',
      onConfirm: () =>
        new Promise(resolve => {
          untrackLogicalModel({
            data: { dataSourceName: m.source, name: m.name },
            onSuccess: () => {
              resolve(true);
            },
            onError: err => {
              hasuraToast({
                type: 'error',
                title: 'Error',
                message: err.message,
              });
              resolve(false);
            },
          });
        }),
    });
  };
  return (
    <div className="flex flex-col">
      <div className="py-md px-md w-full flex">
        <div className="max-w-3xl w-full">
          <Breadcrumbs items={['Data', 'Native Queries']} />
          <div className="text-xl font-bold mt-2">Native Queries</div>
          <div className="text-muted">
            Access more queries and operators through SQL on your database.
          </div>
        </div>
      </div>
      <Tabs
        className="px-md"
        items={[
          {
            value: 'queries',
            label: `Native Queries (${nativeQueries.length})`,
            content: (
              <div className="mt-md">
                <ListNativeQueries
                  nativeQueries={nativeQueries}
                  isLoading={isLoading}
                  onEditClick={() => {
                    hasuraAlert({
                      title: 'Not Implemented',
                      message:
                        'Editing is not implemented in the alpha release',
                    });
                  }}
                  onRemoveClick={handleRemoveNativeQuery}
                />
              </div>
            ),
          },
          {
            value: 'models',
            label: `Logical Models (${logicalModels.length})`,
            content: (
              <div className="mt-md">
                <ListLogicalModels
                  logicalModels={logicalModels}
                  isLoading={isLoading}
                  onEditClick={() => {
                    hasuraAlert({
                      title: 'Not Implemented',
                      message:
                        'Editing is not implemented in the alpha release',
                    });
                  }}
                  onRemoveClick={handleRemoveLogicalModel}
                />
              </div>
            ),
          },
        ]}
      />
    </div>
  );
};
