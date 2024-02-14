import { useState } from 'react';
import { InjectedRouter, Link, withRouter } from 'react-router';
import {
  useDestructiveAlert,
  useHasuraAlert,
} from '../../../../new-components/Alert';
import { Button } from '../../../../new-components/Button';
import { Tabs } from '../../../../new-components/Tabs';
import { hasuraToast } from '../../../../new-components/Toasts';
import {
  useEnvironmentState,
  usePushRoute,
} from '../../../ConnectDBRedesign/hooks';
import { useTrackLogicalModel } from '../../hooks/useTrackLogicalModel';
import { useTrackNativeQuery } from '../../hooks/useTrackNativeQuery';
import { findReferencedEntities } from '../LogicalModel/utils/findReferencedEntities';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';

import { LimitedFeatureWrapper } from '../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { useSyncResourceVersionOnMount } from '../../../hasura-metadata-api';
import { extractModelsAndQueriesFromMetadata } from '../../../hasura-metadata-api/selectors';
import { Metadata } from '../../../hasura-metadata-types';
import { MetadataWrapper } from '../../components';
import { DisplayReferencedLogicalModelEntities } from '../LogicalModel/DisplayLogicalModelReferencedEntities';
import { RouteWrapper } from '../components/RouteWrapper';
import { injectRouteDetails } from '../components/route-wrapper-utils';
import { NATIVE_QUERY_ROUTE_DETAIL, Routes } from '../constants';
import { LogicalModelWithSource, NativeQueryWithSource } from '../types';
import { CodeHighlight } from './components/CodeHighlight';
import { ListLogicalModels } from './components/ListLogicalModels';
import { ListNativeQueries } from './components/ListNativeQueries';
import { ListStoredProcedures } from './components/ListStoredProcedures';

const metadataSelector = (m: Metadata) => {
  const modelsAndQueries = extractModelsAndQueriesFromMetadata(m);
  return {
    storedProcedures: m.metadata.sources
      .map(({ name, stored_procedures }) =>
        (stored_procedures ?? []).map(stored_procedure => ({
          dataSourceName: name,
          ...stored_procedure,
        }))
      )
      .flat(),
    sources: m.metadata.sources,
    nativeQueries: modelsAndQueries?.queries ?? [],
    logicalModels: modelsAndQueries?.models ?? [],
  };
};

type LandingPageData = ReturnType<typeof metadataSelector>;

const DataBoundLandingPage = ({ pathname }: { pathname: string }) => (
  <MetadataWrapper
    selector={metadataSelector}
    render={({ data }) => <LandingPageUI data={data} pathname={pathname} />}
  />
);

// alias for exports
export const LandingPage = DataBoundLandingPage;

export const LandingPageUI = ({
  pathname,
  data: { nativeQueries, logicalModels, storedProcedures, sources },
}: {
  pathname: string;
  data: LandingPageData;
}) => {
  const push = usePushRoute();

  useSyncResourceVersionOnMount({ componentName: 'LandingPage' });

  const { consoleType } = useEnvironmentState();

  const [isLogicalModelsDialogOpen, setIsLogicalModelsDialogOpen] =
    useState(false);

  const { destructiveConfirm } = useDestructiveAlert();
  const { hasuraAlert } = useHasuraAlert();

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
    const entities = findReferencedEntities({
      source: sources.find(s => s.name === m.source.name),
      logicalModelName: m.name,
    });

    if (entities.count > 0) {
      hasuraAlert({
        title: 'Unable To Remove',
        message: (
          <div>
            Logical Model <CodeHighlight>{m.name}</CodeHighlight> is referenced
            by {entities.count} other entity
            {`(s)`}. You cannot remove a Logical Model while it is still being
            referenced.
            <DisplayReferencedLogicalModelEntities entities={entities} />
          </div>
        ),
      });
      return;
    }

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
    <div className="w-full">
      <div className="flex flex-col">
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
          key={pathname}
          defaultValue={pathname}
          onValueChange={value => {
            push?.(value);
          }}
          items={[
            {
              value: Routes.NativeQueries,
              label: `Native Queries (${nativeQueries.length})`,
              content: (
                <div className="mt-md">
                  <ListNativeQueries
                    nativeQueries={nativeQueries}
                    onEditClick={query => {
                      push(
                        injectRouteDetails(Routes.EditNativeQuery, {
                          itemSourceName: query.source.name,
                          itemName: query.root_field_name,
                          itemTabName: 'details',
                        })
                      );
                    }}
                    onRemoveClick={handleRemoveNativeQuery}
                  />
                  <div className="flex justify-end mt-sm">
                    <Link to={Routes.CreateNativeQuery}>
                      <Button mode="primary">Create Native Query</Button>
                    </Link>
                  </div>
                </div>
              ),
            },
            {
              value: Routes.LogicalModels,
              label: `Logical Models (${logicalModels.length})`,
              content: (
                <div className="mt-md">
                  <ListLogicalModels
                    logicalModels={logicalModels}
                    onEditClick={model => {
                      push(
                        injectRouteDetails(Routes.EditLogicalModel, {
                          itemName: model.name,
                          itemSourceName: model.source.name,
                          itemTabName: 'details',
                        })
                      );
                    }}
                    onRemoveClick={handleRemoveLogicalModel}
                  />
                  <div className="flex justify-end mt-sm">
                    <Button
                      mode="primary"
                      onClick={() => {
                        setIsLogicalModelsDialogOpen(true);
                      }}
                    >
                      Add Logical Model
                    </Button>
                  </div>
                </div>
              ),
            },
            ...(consoleType !== 'oss'
              ? [
                  {
                    value: Routes.StoredProcedures,
                    label: `Stored Procedures (${storedProcedures.length})`,
                    content: (
                      <div className="mt-md">
                        <LimitedFeatureWrapper
                          title="Looking to add Stored Procedures for SQL Server?"
                          id="native-queries"
                          description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
                        >
                          <ListStoredProcedures />
                          <div className="flex justify-end mt-sm">
                            <Link to={Routes.TrackStoredProcedure}>
                              <Button mode="primary">
                                Track Stored Procedure
                              </Button>
                            </Link>
                          </div>
                        </LimitedFeatureWrapper>
                      </div>
                    ),
                  },
                ]
              : []),
          ]}
        />
      </div>
    </div>
  );
};

export const LandingPageRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
}>(({ location, router }) => {
  return (
    <RouteWrapper
      route={location.pathname as keyof typeof NATIVE_QUERY_ROUTE_DETAIL}
    >
      <LandingPage pathname={location.pathname} />
    </RouteWrapper>
  );
});
