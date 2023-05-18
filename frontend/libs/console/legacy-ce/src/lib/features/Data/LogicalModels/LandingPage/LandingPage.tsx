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
import { useState } from 'react';
import { InjectedRouter, Link, withRouter } from 'react-router';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';
import { Button } from '../../../../new-components/Button';
import { RouteWrapper } from '../components/RouteWrapper';
import { ListStoredProcedures } from '../StoredProcedures/ListStoredProcedures';

export const LandingPage = ({
  push,
  pathname,
}: {
  pathname: string | undefined;
  push?: (to: string) => void;
}) => {
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
  const nativeQueries = data?.queries ?? [];
  const logicalModels = data?.models ?? [];

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
                    onEditClick={() => {
                      hasuraAlert({
                        title: 'Not Implemented',
                        message:
                          'Editing is not implemented in the alpha release',
                      });
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
                    onEditClick={() => {
                      push?.('/data/native-queries/logical-models/permissions');
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
            {
              value: '/data/native-queries/stored-procedures',
              label: `Stored Procedures (${storedProcedures.length})`,
              content: (
                <div className="mt-md">
                  <ListStoredProcedures />
                  <div className="flex justify-end mt-sm">
                    <Link to="/data/native-queries/stored-procedures/track">
                      <Button mode="primary">Track Stored Procedure</Button>
                    </Link>
                  </div>
                </div>
              ),
            },
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
    <RouteWrapper pathname={location.pathname} push={router.push}>
      <LandingPage pathname={location.pathname} push={router.push} />
    </RouteWrapper>
  );
});
