import { Driver, getSupportedDrivers } from '../../../../dataSources';
import { makeConnectionStringFromConnectionParams } from './ManageDBUtils';
import { addDataSource, renameDataSource } from '../../../../metadata/actions';
import { Dispatch } from '../../../../types';
import {
  SourceConnectionInfo,
  ConnectionPoolSettings,
  SSLModeOptions,
  SSLConfigOptions,
  IsolationLevelOptions,
} from '../../../../metadata/types';

export const connectionTypes = {
  DATABASE_URL: 'DATABASE_URL',
  CONNECTION_PARAMS: 'CONNECTION_PARAMETERS',
  ENV_VAR: 'ENVIRONMENT_VARIABLES',
};

type ConnectionParams = {
  host: string;
  port: string;
  username: string;
  password: string;
  database: string;
};

export type ConnectDBState = {
  displayName: string;
  dbType: Driver;
  connectionParamState: ConnectionParams;
  databaseURLState: {
    dbURL: string;
    serviceAccount: string;
    global_select_limit: number;
    projectId: string;
    datasets: string;
  };
  envVarState: {
    envVar: string;
  };
  connectionSettings?: ConnectionPoolSettings;
  sslConfiguration?: SSLConfigOptions;
  isolationLevel?: IsolationLevelOptions;
  preparedStatements?: boolean;
};

export const defaultState: ConnectDBState = {
  displayName: '',
  dbType: 'postgres',
  connectionParamState: {
    host: '',
    port: '',
    username: '',
    password: '',
    database: '',
  },
  databaseURLState: {
    dbURL: '',
    serviceAccount: '',
    global_select_limit: 1000,
    projectId: '',
    datasets: '',
  },
  envVarState: {
    envVar: '',
  },
  preparedStatements: false,
  isolationLevel: 'read-committed',
};

type DefaultStateProps = {
  dbConnection: {
    dbURL?: string;
    envVar?: string;
    dbName?: string;
  };
};

export const getDefaultState = (props?: DefaultStateProps): ConnectDBState => {
  return {
    ...defaultState,
    displayName: props?.dbConnection.dbName || '',
    databaseURLState: {
      ...defaultState.databaseURLState,
      dbURL: props?.dbConnection.dbURL || '',
    },
    envVarState: {
      envVar: props?.dbConnection.envVar || '',
    },
  };
};

const setNumberFromString = (str: string) => {
  return str ? parseInt(str.trim(), 10) : undefined;
};

const setDataFromEnv = (str: string) => {
  return str
    ? {
        from_env: str,
      }
    : undefined;
};

const checkUndef = (obj?: Record<string, any>) =>
  obj && Object.values(obj).some(el => el !== undefined && el !== null);

const checkEmpty = (obj?: Record<string, any>) =>
  obj && Object.keys(obj).length !== 0 && checkUndef(obj);

export const connectDataSource = (
  dispatch: Dispatch,
  typeConnection: string,
  currentState: ConnectDBState,
  cb: () => void,
  replicas?: Omit<
    SourceConnectionInfo,
    | 'connection_string'
    | 'use_prepared_statements'
    | 'ssl_configuration'
    | 'isolation_level'
  >[],
  isEditState = false,
  isRenameSource = false,
  currentName = ''
) => {
  let databaseURL: string | { from_env: string } =
    currentState.dbType === 'bigquery'
      ? currentState.databaseURLState.serviceAccount.trim()
      : currentState.databaseURLState.dbURL.trim();
  if (
    typeConnection === connectionTypes.ENV_VAR &&
    getSupportedDrivers('connectDbForm.environmentVariable').includes(
      currentState.dbType
    )
  ) {
    databaseURL = { from_env: currentState.envVarState.envVar.trim() };
  } else if (
    typeConnection === connectionTypes.CONNECTION_PARAMS &&
    currentState.dbType !== 'bigquery' &&
    getSupportedDrivers('connectDbForm.connectionParameters').includes(
      currentState.dbType
    )
  ) {
    databaseURL = makeConnectionStringFromConnectionParams({
      dbType: currentState.dbType,
      ...currentState.connectionParamState,
    });
  }

  const data = {
    driver: currentState.dbType,
    payload: {
      name: currentState.displayName.trim(),
      dbUrl: databaseURL,
      replace_configuration: isEditState,
      bigQuery: {
        projectId: currentState.databaseURLState.projectId,
        datasets: currentState.databaseURLState.datasets,
        global_select_limit: currentState.databaseURLState.global_select_limit,
      },
      ...(checkEmpty(currentState.connectionSettings) && {
        connection_pool_settings: currentState.connectionSettings,
      }),
      ...(checkEmpty(currentState.sslConfiguration) && {
        sslConfiguration: currentState.sslConfiguration,
      }),
      preparedStatements: currentState.preparedStatements,
      isolationLevel: currentState.isolationLevel,
    },
  };

  if (isRenameSource) {
    return dispatch(
      renameDataSource(
        data,
        cb,
        { name: currentName, isRenameSource },
        replicas
      )
    );
  }
  return dispatch(addDataSource(data, cb, replicas));
};

export type ConnectDBActions =
  | {
      type: 'INIT';
      data: {
        name: string;
        driver: Driver;
        databaseUrl: string;
        connectionSettings?: ConnectionPoolSettings;
        preparedStatements: boolean;
        isolationLevel: IsolationLevelOptions;
        sslConfiguration?: SSLConfigOptions;
      };
    }
  | { type: 'UPDATE_PARAM_STATE'; data: ConnectionParams }
  | { type: 'UPDATE_DISPLAY_NAME'; data: string }
  | { type: 'UPDATE_DB_URL'; data: string }
  | { type: 'UPDATE_DB_BIGQUERY_SERVICE_ACCOUNT'; data: string }
  | { type: 'UPDATE_DB_BIGQUERY_GLOBAL_LIMIT'; data: number }
  | { type: 'UPDATE_DB_BIGQUERY_PROJECT_ID'; data: string }
  | { type: 'UPDATE_DB_BIGQUERY_DATASETS'; data: string }
  | { type: 'UPDATE_DB_URL_ENV_VAR'; data: string }
  | { type: 'UPDATE_DB_HOST'; data: string }
  | { type: 'UPDATE_DB_PORT'; data: string }
  | { type: 'UPDATE_DB_USERNAME'; data: string }
  | { type: 'UPDATE_DB_PASSWORD'; data: string }
  | { type: 'UPDATE_DB_DATABASE_NAME'; data: string }
  | { type: 'UPDATE_MAX_CONNECTIONS'; data: string }
  | { type: 'UPDATE_RETRIES'; data: string }
  | { type: 'UPDATE_IDLE_TIMEOUT'; data: string }
  | { type: 'UPDATE_POOL_TIMEOUT'; data: string }
  | { type: 'UPDATE_CONNECTION_LIFETIME'; data: string }
  | { type: 'UPDATE_DB_DRIVER'; data: Driver }
  | { type: 'UPDATE_CONNECTION_SETTINGS'; data: ConnectionPoolSettings }
  | { type: 'UPDATE_SSL_MODE'; data: SSLModeOptions }
  | { type: 'UPDATE_SSL_ROOT_CERT'; data: string }
  | { type: 'UPDATE_SSL_CERT'; data: string }
  | { type: 'UPDATE_SSL_KEY'; data: string }
  | { type: 'UPDATE_SSL_PASSWORD'; data: string }
  | { type: 'UPDATE_PREPARED_STATEMENTS'; data: boolean }
  | { type: 'UPDATE_ISOLATION_LEVEL'; data: IsolationLevelOptions }
  | { type: 'RESET_INPUT_STATE' };

export const connectDBReducer = (
  state: ConnectDBState,
  action: ConnectDBActions
): ConnectDBState => {
  switch (action.type) {
    case 'INIT':
      return {
        ...state,
        displayName: action.data.name,
        dbType: action.data.driver,
        databaseURLState: {
          ...state.databaseURLState,
          dbURL: action.data.databaseUrl,
        },
        connectionSettings: action.data.connectionSettings,
        preparedStatements: action.data.preparedStatements,
        isolationLevel: action.data.isolationLevel,
        sslConfiguration: action.data.sslConfiguration,
      };
    case 'UPDATE_PARAM_STATE':
      return {
        ...state,
        connectionParamState: action.data,
      };
    case 'UPDATE_DISPLAY_NAME':
      return {
        ...state,
        displayName: action.data,
      };
    case 'UPDATE_DB_DRIVER':
      return {
        ...state,
        dbType: action.data,
      };
    case 'UPDATE_DB_URL':
      return {
        ...state,
        databaseURLState: {
          ...state.databaseURLState,
          dbURL: action.data,
        },
      };
    case 'UPDATE_DB_URL_ENV_VAR':
      return {
        ...state,
        envVarState: {
          envVar: action.data,
        },
      };
    case 'UPDATE_DB_HOST':
      return {
        ...state,
        connectionParamState: {
          ...state.connectionParamState,
          host: action.data,
        },
      };
    case 'UPDATE_DB_PORT':
      return {
        ...state,
        connectionParamState: {
          ...state.connectionParamState,
          port: action.data,
        },
      };
    case 'UPDATE_DB_USERNAME':
      return {
        ...state,
        connectionParamState: {
          ...state.connectionParamState,
          username: action.data,
        },
      };
    case 'UPDATE_DB_PASSWORD':
      return {
        ...state,
        connectionParamState: {
          ...state.connectionParamState,
          password: action.data,
        },
      };
    case 'UPDATE_DB_DATABASE_NAME':
      return {
        ...state,
        connectionParamState: {
          ...state.connectionParamState,
          database: action.data,
        },
      };
    case 'RESET_INPUT_STATE':
      return {
        ...defaultState,
      };
    case 'UPDATE_MAX_CONNECTIONS':
      return {
        ...state,
        connectionSettings: {
          ...state.connectionSettings,
          max_connections: setNumberFromString(action.data),
        },
      };
    case 'UPDATE_RETRIES':
      return {
        ...state,
        connectionSettings: {
          ...state.connectionSettings,
          retries: setNumberFromString(action.data),
        },
      };
    case 'UPDATE_IDLE_TIMEOUT':
      return {
        ...state,
        connectionSettings: {
          ...state.connectionSettings,
          idle_timeout: setNumberFromString(action.data),
        },
      };
    case 'UPDATE_POOL_TIMEOUT':
      return {
        ...state,
        connectionSettings: {
          ...state.connectionSettings,
          pool_timeout: setNumberFromString(action.data),
        },
      };
    case 'UPDATE_CONNECTION_LIFETIME':
      return {
        ...state,
        connectionSettings: {
          ...state.connectionSettings,
          connection_lifetime: setNumberFromString(action.data),
        },
      };
    case 'UPDATE_CONNECTION_SETTINGS':
      return {
        ...state,
        connectionSettings: action.data,
      };
    case 'UPDATE_SSL_MODE':
      return {
        ...state,
        sslConfiguration: {
          ...state.sslConfiguration,
          sslmode: action.data,
        },
      };
    case 'UPDATE_SSL_ROOT_CERT':
      return {
        ...state,
        sslConfiguration: {
          ...state.sslConfiguration,
          sslrootcert: setDataFromEnv(action.data),
        },
      };
    case 'UPDATE_SSL_CERT':
      return {
        ...state,
        sslConfiguration: {
          ...state.sslConfiguration,
          sslcert: setDataFromEnv(action.data),
        },
      };
    case 'UPDATE_SSL_KEY':
      return {
        ...state,
        sslConfiguration: {
          ...state.sslConfiguration,
          sslkey: setDataFromEnv(action.data),
        },
      };
    case 'UPDATE_SSL_PASSWORD':
      return {
        ...state,
        sslConfiguration: {
          ...state.sslConfiguration,
          sslpassword: setDataFromEnv(action.data),
        },
      };
    case 'UPDATE_ISOLATION_LEVEL':
      return {
        ...state,
        isolationLevel: action.data,
      };
    case 'UPDATE_PREPARED_STATEMENTS':
      return {
        ...state,
        preparedStatements: action.data,
      };
    case 'UPDATE_DB_BIGQUERY_SERVICE_ACCOUNT':
      return {
        ...state,
        databaseURLState: {
          ...state.databaseURLState,
          serviceAccount: action.data,
        },
      };
    case 'UPDATE_DB_BIGQUERY_GLOBAL_LIMIT':
      return {
        ...state,
        databaseURLState: {
          ...state.databaseURLState,
          global_select_limit: action.data,
        },
      };
    case 'UPDATE_DB_BIGQUERY_DATASETS':
      return {
        ...state,
        databaseURLState: {
          ...state.databaseURLState,
          datasets: action.data,
        },
      };
    case 'UPDATE_DB_BIGQUERY_PROJECT_ID':
      return {
        ...state,
        databaseURLState: {
          ...state.databaseURLState,
          projectId: action.data,
        },
      };
    default:
      return state;
  }
};

export interface ExtendedConnectDBState extends ConnectDBState {
  chosenConnectionType: string;
}

const defaultReadReplicasState: ExtendedConnectDBState[] = [];
export type ReadReplicaState = ExtendedConnectDBState[];

export interface AddReadReplicaToState {
  type: 'ADD_READ_REPLICA';
  data: ExtendedConnectDBState;
}

export interface RemoveReadReplicaFromState {
  type: 'REMOVE_READ_REPLICA';
  // the default name is set using index `read-replica-${index}`
  // we can use that to simplify removal
  data: string;
}

export interface ResetReadReplicaState {
  type: 'RESET_READ_REPLICA_STATE';
}

export type ReadReplicaActions =
  | AddReadReplicaToState
  | RemoveReadReplicaFromState
  | ResetReadReplicaState;

export const readReplicaReducer = (
  state: ReadReplicaState,
  action: ReadReplicaActions
): ReadReplicaState => {
  switch (action.type) {
    case 'ADD_READ_REPLICA':
      return [...state, action.data];
    case 'REMOVE_READ_REPLICA':
      return state.filter(st => st.displayName !== action.data);
    case 'RESET_READ_REPLICA_STATE':
      return defaultReadReplicasState;
    default:
      return state;
  }
};

export const makeReadReplicaConnectionObject = (
  stateVal: ExtendedConnectDBState
) => {
  let database_url;
  if (stateVal.chosenConnectionType === connectionTypes.DATABASE_URL) {
    database_url = stateVal.databaseURLState?.dbURL?.trim() ?? '';
  } else if (stateVal.chosenConnectionType === connectionTypes.ENV_VAR) {
    database_url = {
      from_env: stateVal.envVarState?.envVar?.trim() ?? '',
    };
  } else {
    database_url = makeConnectionStringFromConnectionParams({
      dbType: 'postgres',
      ...stateVal.connectionParamState,
    });
  }

  const pool_settings: any = {};
  if (stateVal.connectionSettings?.max_connections) {
    pool_settings.max_connections = stateVal.connectionSettings.max_connections;
  }
  if (stateVal.connectionSettings?.idle_timeout) {
    pool_settings.idle_timeout = stateVal.connectionSettings.idle_timeout;
  }
  if (stateVal.connectionSettings?.retries) {
    pool_settings.retries = stateVal.connectionSettings.retries;
  }

  return {
    database_url,
    pool_settings,
  };
};
