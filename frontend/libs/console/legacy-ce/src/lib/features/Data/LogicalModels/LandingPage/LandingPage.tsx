import React, { useEffect, useState } from 'react';
import { InjectedRouter, Link, withRouter } from 'react-router';
import { useDestructiveAlert } from '../../../../new-components/Alert';
import { Button } from '../../../../new-components/Button';
import { Tabs } from '../../../../new-components/Tabs';
import { hasuraToast } from '../../../../new-components/Toasts';
import {
  useEnvironmentState,
  usePushRoute,
} from '../../../ConnectDBRedesign/hooks';
import {
  useInvalidateMetadata,
  useMetadata,
} from '../../../hasura-metadata-api';
import { useTrackLogicalModel } from '../../hooks/useTrackLogicalModel';
import { useTrackNativeQuery } from '../../hooks/useTrackNativeQuery';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';

import { LogicalModelWithSource, NativeQueryWithSource } from '../types';
import { ListLogicalModels } from './components/ListLogicalModels';
import { ListNativeQueries } from './components/ListNativeQueries';
import { ListStoredProcedures } from './components/ListStoredProcedures';
import { NATIVE_QUERY_ROUTES } from '../constants';
import { extractModelsAndQueriesFromMetadata } from '../../../hasura-metadata-api/selectors';
import { RouteWrapper } from '../components/RouteWrapper';

export const LandingPage = ({ pathname }: { pathname: string }) => {
  const push = usePushRoute();
  const { data, isLoading } = useMetadata(m =>
    extractModelsAndQueriesFromMetadata(m)
  );
  const { data: storedProcedures = [] } = useMetadata(m =>
    m.metadata.sources
      .map(({ name, stored_procedures }) =>
        (stored_procedures ?? []).map(stored_procedure => ({
          dataSourceName: name,
          ...stored_procedure,
        }))
      )
      .flat()
  );

  const invalidateMetadata = useInvalidateMetadata();
  useEffect(() => {
    /**
     * Workaround to avoid that a metadata migration that happened in the legacy part of the Console (i.e. Run SQL)
     * might affect the metadata migrations in the child components of this page,
     * resulting in the error "metadata resource version referenced (x) did not match current version"
     */
    invalidateMetadata();
  }, []);

  const nativeQueries = data?.queries ?? [];
  const logicalModels = data?.models ?? [];
  const { consoleType } = useEnvironmentState();

  const [isLogicalModelsDialogOpen, setIsLogicalModelsDialogOpen] =
    useState(false);

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
              value: '/data/native-queries',
              label: `Native Queries (${nativeQueries.length})`,
              content: (
                <div className="mt-md">
                  <ListNativeQueries
                    nativeQueries={nativeQueries}
                    isLoading={isLoading}
                    onEditClick={query => {
                      push?.(
                        `data/native-queries/native-query/${query.source.name}/${query.root_field_name}`
                      );
                    }}
                    onRemoveClick={handleRemoveNativeQuery}
                  />
                  <div className="flex justify-end mt-sm">
                    <Link to="/data/native-queries/create">
                      <Button mode="primary">Create Native Query</Button>
                    </Link>
                  </div>
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
                    onEditClick={model => {
                      push?.(
                        `/data/native-queries/logical-models/${model.source.name}/${model.name}/permissions`
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
                    value: '/data/native-queries/stored-procedures',
                    label: `Stored Procedures (${storedProcedures.length})`,
                    content: (
                      <div className="mt-md">
                        <ListStoredProcedures />
                        <div className="flex justify-end mt-sm">
                          <Link to="/data/native-queries/stored-procedures/track">
                            <Button mode="primary">
                              Track Stored Procedure
                            </Button>
                          </Link>
                        </div>
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
    <RouteWrapper route={location.pathname as keyof typeof NATIVE_QUERY_ROUTES}>
      <LandingPage pathname={location.pathname} />
    </RouteWrapper>
  );
});
