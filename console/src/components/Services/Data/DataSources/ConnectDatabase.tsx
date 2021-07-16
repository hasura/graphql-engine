/* eslint-disable no-underscore-dangle */
import React, {
  ChangeEvent,
  FormEvent,
  useReducer,
  useState,
  useEffect,
} from 'react';
import { connect, ConnectedProps } from 'react-redux';

import Tabbed from './TabbedDataSourceConnection';
import { ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { showErrorNotification } from '../../Common/Notification';
import _push from '../push';
import {
  connectDataSource,
  connectDBReducer,
  connectionTypes,
  getDefaultState,
  readReplicaReducer,
  defaultState,
  makeReadReplicaConnectionObject,
} from './state';
import {
  getDatasourceURL,
  getErrorMessageFromMissingFields,
  parsePgUrl,
} from './utils';
import ReadReplicaForm from './ReadReplicaForm';
import EditDataSource from './EditDataSource';
import DataSourceFormWrapper from './DataSourceFromWrapper';
import { getSupportedDrivers } from '../../../../dataSources';

interface ConnectDatabaseProps extends InjectedProps {}

const ConnectDatabase: React.FC<ConnectDatabaseProps> = props => {
  const { dispatch } = props;

  const [connectDBInputState, connectDBDispatch] = useReducer(
    connectDBReducer,
    getDefaultState(props)
  );

  const [connectionType, changeConnectionType] = useState(
    props.dbConnection.envVar
      ? connectionTypes.ENV_VAR
      : connectionTypes.DATABASE_URL
  );

  const [loading, setLoading] = useState(false);

  const [readReplicasState, readReplicaDispatch] = useReducer(
    readReplicaReducer,
    []
  );
  const [
    connectDBStateForReadReplica,
    connectDBReadReplicaDispatch,
  ] = useReducer(connectDBReducer, defaultState);
  const [readReplicaConnectionType, updateReadReplicaConnectionType] = useState(
    connectionTypes.DATABASE_URL
  );

  const { sources = [], pathname } = props;
  const isEditState =
    pathname.includes('edit') || pathname.indexOf('edit') !== -1;
  const paths = pathname.split('/');
  const editSourceName = paths[paths.length - 1];
  const currentSourceInfo = sources.find(
    source => source.name === editSourceName
  );

  useEffect(() => {
    if (isEditState && currentSourceInfo) {
      const connectionInfo = currentSourceInfo.configuration?.connection_info;
      const databaseUrl =
        connectionInfo?.database_url || connectionInfo?.connection_string;
      connectDBDispatch({
        type: 'INIT',
        data: {
          name: currentSourceInfo.name,
          driver: currentSourceInfo.kind ?? 'postgres',
          databaseUrl: getDatasourceURL(
            databaseUrl ?? connectionInfo?.connection_string
          ),
          connectionSettings: connectionInfo?.pool_settings,
          preparedStatements: connectionInfo?.use_prepared_statements ?? false,
          isolationLevel: connectionInfo?.isolation_level ?? 'read-committed',
          sslConfiguration: connectionInfo?.ssl_configuration,
        },
      });

      if (
        typeof databaseUrl === 'string' &&
        currentSourceInfo.kind === 'postgres'
      ) {
        const p = parsePgUrl(databaseUrl);
        connectDBDispatch({
          type: 'UPDATE_PARAM_STATE',
          data: {
            host: p.host?.replace(/:\d*$/, '') ?? '',
            port: p.port ?? '',
            database: p.pathname?.slice(1) ?? '',
            username: p.username ?? '',
            password: p.password ?? '',
          },
        });
      }

      if (typeof databaseUrl !== 'string' && databaseUrl?.from_env) {
        changeConnectionType(connectionTypes.ENV_VAR);
        connectDBDispatch({
          type: 'UPDATE_DB_URL_ENV_VAR',
          data: databaseUrl.from_env,
        });
        connectDBDispatch({
          type: 'UPDATE_DB_URL',
          data: '',
        });
      }

      if (currentSourceInfo?.kind === 'bigquery') {
        const conf = currentSourceInfo.configuration;
        connectDBDispatch({
          type: 'UPDATE_DB_BIGQUERY_DATASETS',
          data: conf?.datasets?.join(', ') ?? '',
        });
        connectDBDispatch({
          type: 'UPDATE_DB_BIGQUERY_PROJECT_ID',
          data: conf?.project_id ?? '',
        });
        if (conf?.global_select_limit) {
          connectDBDispatch({
            type: 'UPDATE_DB_BIGQUERY_GLOBAL_LIMIT',
            data: +conf?.global_select_limit,
          });
        }
        if (conf?.service_account?.from_env) {
          changeConnectionType(connectionTypes.ENV_VAR);
          connectDBDispatch({
            type: 'UPDATE_DB_URL_ENV_VAR',
            data: conf?.service_account?.from_env,
          });
        } else {
          changeConnectionType(connectionTypes.CONNECTION_PARAMS);
          connectDBDispatch({
            type: 'UPDATE_DB_BIGQUERY_SERVICE_ACCOUNT',
            data: JSON.stringify(conf?.service_account, null, 2) ?? '{}',
          });
        }
      }
    }
  }, [isEditState, currentSourceInfo]);

  const onChangeConnectionType = (e: ChangeEvent<HTMLInputElement>) =>
    changeConnectionType(e.target.value);

  const resetState = () => {
    connectDBDispatch({
      type: 'RESET_INPUT_STATE',
    });
    dispatch(_push('/data/manage'));
  };

  const onSuccessConnectDBCb = () => {
    setLoading(false);
    resetState();
    // route to manage page
    dispatch(_push('/data/manage'));
  };

  const onConnectDatabase = () => {
    if (!connectDBInputState.displayName.trim()) {
      dispatch(
        showErrorNotification(
          'Display Name is a mandatory field',
          'Please enter a valid display name'
        )
      );
      return;
    }

    // TODO: check if permitted, if not pass undefined
    const read_replicas = readReplicasState.map(replica =>
      makeReadReplicaConnectionObject(replica)
    );

    const isRenameSource =
      isEditState && editSourceName !== connectDBInputState.displayName.trim();

    if (
      connectionType === connectionTypes.DATABASE_URL ||
      (connectionType === connectionTypes.CONNECTION_PARAMS &&
        connectDBInputState.dbType === 'mssql')
    ) {
      if (
        !connectDBInputState.databaseURLState.dbURL.trim() &&
        connectDBInputState.dbType !== 'bigquery'
      ) {
        dispatch(
          showErrorNotification(
            'Database URL is a mandatory field',
            'Please enter a valid database URL'
          )
        );
        return;
      }

      setLoading(true);
      connectDataSource(
        dispatch,
        connectionTypes.DATABASE_URL,
        connectDBInputState,
        onSuccessConnectDBCb,
        read_replicas,
        isEditState,
        isRenameSource,
        editSourceName
      )
        .then(() => setLoading(false))
        .catch(() => setLoading(false));
      return;
    }

    if (connectionType === connectionTypes.ENV_VAR) {
      if (
        !connectDBInputState.envVarState.envVar.trim() &&
        connectDBInputState.dbType !== 'bigquery'
      ) {
        dispatch(
          showErrorNotification(
            'Environment Variable is a mandatory field',
            'Please enter the name of a valid environment variable'
          )
        );
        return;
      }

      setLoading(true);
      connectDataSource(
        dispatch,
        connectionTypes.ENV_VAR,
        connectDBInputState,
        onSuccessConnectDBCb,
        read_replicas,
        isEditState,
        isRenameSource,
        editSourceName
      )
        .then(() => setLoading(false))
        .catch(() => setLoading(false));
      return;
    }

    // construct the connection string from connection params and
    // make the same call as done for connection of type DATABASE_URL
    const {
      host,
      port,
      username,
      database,
    } = connectDBInputState.connectionParamState;

    if (connectDBInputState.dbType !== 'bigquery') {
      if (!host || !port || !username || !database) {
        const errorMessage = getErrorMessageFromMissingFields(
          host,
          port,
          username,
          database
        );
        dispatch(
          showErrorNotification('Required fields are missing', errorMessage)
        );
        return;
      }
    }
    setLoading(true);
    connectDataSource(
      dispatch,
      connectionTypes.CONNECTION_PARAMS,
      connectDBInputState,
      onSuccessConnectDBCb,
      read_replicas,
      isEditState,
      isRenameSource,
      editSourceName
    )
      .then(() => setLoading(false))
      .catch(() => setLoading(false));
  };

  const onSubmit = (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    onConnectDatabase();
  };

  const updateRadioForReadReplica = (e: ChangeEvent<HTMLInputElement>) =>
    updateReadReplicaConnectionType(e.target.value);

  const onClickAddReadReplica = () => {
    connectDBReadReplicaDispatch({
      type: 'RESET_INPUT_STATE',
    });
    updateReadReplicaConnectionType(connectionTypes.DATABASE_URL);
    const indexForName = readReplicasState.length;
    connectDBReadReplicaDispatch({
      type: 'UPDATE_DISPLAY_NAME',
      data: `read-replica-${indexForName}`,
    });
  };

  const onClickCancelOnReadReplicaForm = () =>
    connectDBReadReplicaDispatch({
      type: 'RESET_INPUT_STATE',
    });

  const onClickSaveReadReplicaForm = () => {
    readReplicaDispatch({
      type: 'ADD_READ_REPLICA',
      data: {
        ...connectDBStateForReadReplica,
        chosenConnectionType: readReplicaConnectionType,
      },
    });
    connectDBReadReplicaDispatch({
      type: 'RESET_INPUT_STATE',
    });
  };

  if (isEditState) {
    return (
      <EditDataSource>
        <DataSourceFormWrapper
          connectionDBState={connectDBInputState}
          connectionDBStateDispatch={connectDBDispatch}
          connectionTypeState={connectionType}
          updateConnectionTypeRadio={onChangeConnectionType}
          changeConnectionType={changeConnectionType}
          isEditState={isEditState}
          loading={loading}
          onSubmit={onSubmit}
          title="Edit Data Source"
        />
      </EditDataSource>
    );
  }

  return (
    <Tabbed tabName="connect">
      <DataSourceFormWrapper
        connectionDBState={connectDBInputState}
        connectionDBStateDispatch={connectDBDispatch}
        connectionTypeState={connectionType}
        updateConnectionTypeRadio={onChangeConnectionType}
        changeConnectionType={changeConnectionType}
        isEditState={isEditState}
        loading={loading}
        onSubmit={onSubmit}
      >
        {/* Should be rendered only on Pro and Cloud Console */}
        {getSupportedDrivers('connectDbForm.read_replicas').includes(
          connectDBInputState.dbType
        ) &&
          (window.__env.consoleId || window.__env.userRole) && (
            <ReadReplicaForm
              readReplicaState={readReplicasState}
              readReplicaDispatch={readReplicaDispatch}
              connectDBState={connectDBStateForReadReplica}
              connectDBStateDispatch={connectDBReadReplicaDispatch}
              readReplicaConnectionType={readReplicaConnectionType}
              updateReadReplicaConnectionType={updateRadioForReadReplica}
              onClickAddReadReplicaCb={onClickAddReadReplica}
              onClickCancelOnReadReplicaCb={onClickCancelOnReadReplicaForm}
              onClickSaveReadReplicaCb={onClickSaveReadReplicaForm}
            />
          )}
      </DataSourceFormWrapper>
    </Tabbed>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    currentDataSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
    sources: state.metadata.metadataObject?.sources ?? [],
    dbConnection: state.tables.dbConnection,
    pathname: state?.routing?.locationBeforeTransitions?.pathname ?? '',
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedDatabaseConnectPage = connector(ConnectDatabase);
export default ConnectedDatabaseConnectPage;
