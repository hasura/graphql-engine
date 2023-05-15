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
import { ReactNode, useState } from 'react';
import { InjectedRouter, Link, withRouter } from 'react-router';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';
import { Button } from '../../../../new-components/Button';
import startCase from 'lodash/startCase';
import { LimitedFeatureWrapper } from '../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { useServerConfig } from '../../../../hooks';
import Skeleton from 'react-loading-skeleton';

export const LandingPage = ({
  pathname,
  push,
}: {
  pathname: string | undefined;
  push?: (to: string) => void;
}) => {
  const { data, isLoading } = useMetadata(m =>
    extractModelsAndQueriesFromMetadata(m)
  );
  const nativeQueries = data?.queries ?? [];
  const logicalModels = data?.models ?? [];
  const paths = pathname?.split('/').filter(Boolean) ?? [];
  const isLogicalModelsTab = paths[paths.length - 1] === 'logical-models';
  const title = isLogicalModelsTab ? 'Logical Models' : 'Native Queries';
  const subtitle = isLogicalModelsTab
    ? 'Creating Logical Models in advance can help generate Native Queries faster'
    : 'Access more queries and operators through SQL on your database.';
  const [isLogicalModelsDialogOpen, setIsLogicalModelsDialogOpen] =
    useState(false);

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
            data: {
              dataSourceName: m.source.name,
              name: m.name,
              dataSourceKind: m.source.kind,
            },
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
    <div className="py-md px-md w-full">
      <LimitedFeatureWrapper
        title="Looking to add Native Queries?"
        id="native-queries"
        description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
      >
        <NativeQueriesFeatureFlag>
          <div className="flex flex-col">
            <Breadcrumbs
              items={paths.map((path: string, index) => {
                return {
                  title: startCase(path),
                  onClick:
                    index === paths.length - 1
                      ? undefined
                      : () => {
                          push?.(paths.slice(0, index + 1).join('/'));
                        },
                };
              })}
            />
            <div className="flex w-full justify-between">
              <div>
                <div className="text-xl font-bold mt-2">{title}</div>
                <div className="text-muted">{subtitle}</div>
              </div>
              <div className="items-end flex gap-2">
                <Button
                  onClick={() => {
                    setIsLogicalModelsDialogOpen(true);
                  }}
                >
                  Add Logical Model
                </Button>
                <Link to="/data/native-queries/create">
                  <Button mode="primary">Create Native Query</Button>
                </Link>
              </div>
            </div>
            {isLogicalModelsDialogOpen ? (
              <LogicalModelWidget
                onCancel={() => {
                  setIsLogicalModelsDialogOpen(false);
                }}
                onSubmit={() => {
                  setIsLogicalModelsDialogOpen(false);
                }}
                asDialog
              />
            ) : null}
            <Tabs
              defaultValue={pathname}
              className="px-md"
              onValueChange={value => {
                push?.(value);
              }}
              items={[
                {
                  value: '/data/native-queries',
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
                  value: '/data/native-queries/logical-models',
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
        </NativeQueriesFeatureFlag>
      </LimitedFeatureWrapper>
    </div>
  );
};

function NativeQueriesFeatureFlag({ children }: { children: ReactNode }) {
  const { data: serverConfig, isLoading: isLoadingServerConfig } =
    useServerConfig();
  if (isLoadingServerConfig) {
    return <Skeleton count={2} />;
  }
  const areNativeQueriesEnabled = serverConfig?.feature_flags?.find(
    feature => feature.name === 'native-query-interface' && feature.enabled
  );
  if (!areNativeQueriesEnabled) {
    return (
      <div className="bg-white border-2 shadow-sm p-5 rounded space-y-4">
        <div className="text-xl text-slate-900">
          Looking to try Native Queries?
        </div>
        <div className="text-base text-muted max-w-2xl">
          Native Queries are not enabled. To enable them, start the Hasura
          server with the environment variable
          <code>HASURA_FF_NATIVE_QUERY_INTERFACE: 'True'</code>
        </div>
      </div>
    );
  }
  return children as JSX.Element;
}

export const LandingPageRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
}>(({ location, router }) => {
  return <LandingPage pathname={location.pathname} push={router.push} />;
});
