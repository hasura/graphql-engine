import { Driver, getSupportedDrivers } from '../../../../dataSources';
import { makeConnectionStringFromConnectionParams } from './ManageDBUtils';
import { addDataSource } from '../../../../metadata/actions';
import { Dispatch } from '../../../../types';
import { SourceConnectionInfo } from '../../../../metadata/types';

export const connectionTypes = {
  DATABASE_URL: 'DATABASE_URL',
  CONNECTION_PARAMS: 'CONNECTION_PARAMETERS',
  ENV_VAR: 'ENVIRONMENT_VARIABLES',
};

type ConnectionSettings = {
  max_connections?: number;
  idle_timeout?: number;
  retries?: number;
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
    serviceAccountFile: string;
    projectId: string;
    datasets: string;
  };
  envVarState: {
    envVar: string;
  };
  connectionSettings: ConnectionSettings;
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
    serviceAccountFile: '',
    projectId: '',
    datasets: '',
  },
  envVarState: {
    envVar: '',
  },
  connectionSettings: {},
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
  return parseInt(str.trim(), 10);
};

export const connectDataSource = (
  dispatch: Dispatch,
  typeConnection: string,
  currentState: ConnectDBState,
  cb: () => void,
  replicas?: Omit<SourceConnectionInfo, 'connection_string'>[]
) => {
  let databaseURL: string | { from_env: string } =
    currentState.dbType === 'bigquery'
      ? currentState.databaseURLState.serviceAccountFile.trim()
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
    getSupportedDrivers('connectDbForm.connectionParameters').includes(
      currentState.dbType
    )
  ) {
    databaseURL = makeConnectionStringFromConnectionParams({
      dbType: currentState.dbType,
      ...currentState.connectionParamState,
    });
  }

  return dispatch(
    addDataSource(
      {
        driver: currentState.dbType,
        payload: {
          name: currentState.displayName.trim(),
          dbUrl: databaseURL,
          connection_pool_settings: currentState.connectionSettings,
          bigQuery: {
            projectId: currentState.databaseURLState.projectId,
            datasets: currentState.databaseURLState.datasets,
          },
        },
      },
      cb,
      replicas
    )
  );
};

export type ConnectDBActions =
  | {
      type: 'INIT';
      data: {
        name: string;
        driver: Driver;
        databaseUrl: string;
        connectionSettings: ConnectionSettings;
      };
    }
  | { type: 'UPDATE_DISPLAY_NAME'; data: string }
  | { type: 'UPDATE_DB_URL'; data: string }
  | { type: 'UPDATE_DB_BIGQUERY_SERVICE_ACCOUNT_FILE'; data: string }
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
  | { type: 'UPDATE_DB_DRIVER'; data: Driver }
  | { type: 'UPDATE_CONNECTION_SETTINGS'; data: ConnectionSettings }
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
    case 'UPDATE_CONNECTION_SETTINGS':
      return {
        ...state,
        connectionSettings: action.data,
      };
    case 'UPDATE_DB_BIGQUERY_SERVICE_ACCOUNT_FILE':
      return {
        ...state,
        databaseURLState: {
          ...state.databaseURLState,
          serviceAccountFile: action.data,
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
  if (stateVal.connectionSettings.max_connections) {
    pool_settings.max_connections = stateVal.connectionSettings.max_connections;
  }
  if (stateVal.connectionSettings.idle_timeout) {
    pool_settings.idle_timeout = stateVal.connectionSettings.idle_timeout;
  }
  if (stateVal.connectionSettings.retries) {
    pool_settings.retries = stateVal.connectionSettings.retries;
  }

  return {
    database_url,
    pool_settings,
  };
};
