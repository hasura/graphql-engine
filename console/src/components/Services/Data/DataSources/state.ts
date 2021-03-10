import { Driver } from '../../../../dataSources';
import { makeConnectionStringFromConnectionParams } from './ManageDBUtils';
import { addDataSource } from '../../../../metadata/actions';
import { Dispatch } from '../../../../types';

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
  };
  envVarURLState: {
    envVarURL: string;
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
  },
  envVarURLState: {
    envVarURL: '',
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
      dbURL: props?.dbConnection.dbURL || '',
    },
    envVarURLState: {
      envVarURL: props?.dbConnection.envVar || '',
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
  cb: () => void
) => {
  let databaseURL:
    | string
    | { from_env: string } = currentState.databaseURLState.dbURL.trim();
  if (typeConnection === connectionTypes.ENV_VAR) {
    databaseURL = { from_env: currentState.envVarURLState.envVarURL.trim() };
  } else if (typeConnection === connectionTypes.CONNECTION_PARAMS) {
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
        },
      },
      cb
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
          dbURL: action.data,
        },
      };
    case 'UPDATE_DB_URL_ENV_VAR':
      return {
        ...state,
        envVarURLState: {
          envVarURL: action.data,
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
    default:
      return state;
  }
};
