import { useCallback, useState } from 'react';
import { programmaticallyTraceError } from '../../../../../../features/Analytics';
import { Dispatch } from '../../../../../../types';
import {
  setDBURLInEnvVars,
  verifyProjectHealthAndConnectDataSource,
} from '../utils';
import { NeonIntegrationContext } from './utils';
import { setDBConnectionDetails } from '../../../DataActions';
import {
  connectDataSource,
  connectionTypes,
  getDefaultState,
} from '../../../DataSources/state';
import Globals from '../../../../../../Globals';
import {
  controlPlaneClient,
  FETCH_CONFIG_STATUS,
  FetchConfigStatusSubscription,
} from '../../../../../../features/ControlPlane';

type HasuraDBCreationPayload = {
  envVar: string;
  dataSourceName: string;
};

// this hook must never go into an error
type HasuraDatasourceStatus =
  | {
      status: 'idle';
    }
  | {
      status: 'adding-env-var';
    }
  | {
      status: 'adding-data-source';
      payload: HasuraDBCreationPayload;
    }
  | {
      status: 'success';
      payload: HasuraDBCreationPayload;
    }
  | {
      status: 'adding-env-var-failed';
      payload: {
        dbUrl: string;
      };
    }
  | {
      status: 'adding-data-source-failed';
      payload: HasuraDBCreationPayload;
    };

export function useCreateHasuraCloudDatasource(
  dbUrl: string,
  dataSourceName = 'default',
  dispatch: Dispatch,
  context: NeonIntegrationContext
) {
  const [state, setState] = useState<HasuraDatasourceStatus>({
    status: 'idle',
  });

  // this function adds an ENV var database to Hasura
  const executeConnect = useCallback(
    (
      envVar: string,
      successCallback: VoidFunction,
      errorCallback: VoidFunction
    ) => {
      setState({
        status: 'adding-data-source',
        payload: {
          envVar,
          dataSourceName,
        },
      });

      const connectionConfig = {
        envVar,
        dbName: dataSourceName,
      };

      dispatch(setDBConnectionDetails(connectionConfig));
      try {
        connectDataSource(
          dispatch,
          connectionTypes.ENV_VAR,
          getDefaultState({
            dbConnection: connectionConfig,
          }),
          successCallback,
          undefined,
          undefined,
          undefined,
          undefined,
          context === 'data-manage-create'
        );
      } catch (e) {
        errorCallback();
      }
    },
    [dataSourceName, dbUrl]
  );

  const start = useCallback(() => {
    if (dbUrl) {
      setState({
        status: 'adding-env-var',
      });

      // This sets the database URL of the given Hasura project as an env var in Hasura Cloud project
      setDBURLInEnvVars(dbUrl)
        .then(data => {
          const { envVar, oldConfigHash } = data;
          setState({
            status: 'adding-data-source',
            payload: {
              envVar,
              dataSourceName,
            },
          });

          const successCallback = () => {
            executeConnect(
              envVar,
              // set success status when the data source gets added successfully
              () => {
                setState({
                  status: 'success',
                  payload: {
                    envVar,
                    dataSourceName,
                  },
                });
              },
              // set error status if creating env var failed
              () => {
                setState({
                  status: 'adding-data-source-failed',
                  payload: {
                    envVar,
                    dataSourceName,
                  },
                });
              }
            );
          };
          const errorCallback = () => {
            setState({
              status: 'adding-data-source-failed',
              payload: {
                envVar,
                dataSourceName,
              },
            });
          };

          /**
           * Getting a success response on adding an env var for a tenant on lux does not
           * mean that the change has propagated to all workers of that tenant.
           *
           * If the change has not propagated to a worker to which the `pg_add_source` request is routed to,
           * the server would throw an `inconsistent_object` error as the new env var will not be found.
           *
           * To handle this, we wait for the changes to be propagated to all the live hasura
           * workers of a tenant before attempting to connect the DB.
           *
           * We do that by verifying that the new config hash has been set
           * for all the live workers of the tenant using the `config_status` table
           *
           * Hash is compared against the `config_status` table and worker status
           * is checked against the `hasura_worker` table
           *
           * Additionally add a timeout interval with retries as an additional
           * redundancy since it throws CORS error locally
           */
          const { unsubscribe } =
            controlPlaneClient.subscribe<FetchConfigStatusSubscription>(
              FETCH_CONFIG_STATUS,
              {
                tenantId: Globals.hasuraCloudTenantId,
              },
              data => {
                if (
                  // check if all workers are successfully configured with the new hash
                  data.config_status.every(
                    config =>
                      config.message === 'Service configured successfully' &&
                      config.hash !== oldConfigHash
                  )
                ) {
                  verifyProjectHealthAndConnectDataSource(
                    successCallback,
                    errorCallback
                  );
                  unsubscribe();
                }
              },
              error => {
                programmaticallyTraceError({
                  error:
                    'failed subscribing to fetch_config_status while connecting neon database',
                  cause: error,
                });
                errorCallback();
                unsubscribe();
              }
            );
        })
        .catch(error => {
          // if adding env var fails unexpectedly, set the error state
          setState(prevState => {
            if (prevState.status === 'adding-env-var') {
              // this is an unexpected error; so we need alerts about this
              programmaticallyTraceError({
                error: 'Failed creating env vars in Hasura',
                cause: error,
              });
              return {
                status: 'adding-env-var-failed',
                payload: { dbUrl },
              };
              // if adding data-source fails unexpectedly, set the error state
            } else if (prevState.status === 'adding-data-source') {
              // this is an unexpected error; so we need alerts about this
              programmaticallyTraceError({
                error: 'Failed adding created data source in Hasura',
                cause: error,
              });

              return {
                status: 'adding-data-source-failed',
                payload: prevState.payload,
              };
            }
            return prevState;
          });
        });
    }
  }, [dbUrl, dataSourceName, executeConnect]);

  return {
    state,
    addHasuraDatasource: start,
  };
}
