import { Driver, sourceNames } from '../dataSources';
import {
  ConnectionParams,
  ConnectionPoolSettings,
  GraphQLFieldCustomization,
  IsolationLevelOptions,
  SourceConnectionInfo,
  SSLConfigOptions,
} from './types';

type CustomizationPayloadSlice = {
  customization: {
    root_fields?: GraphQLFieldCustomization['rootFields'];
    type_names?: GraphQLFieldCustomization['typeNames'];
    naming_convention?: GraphQLFieldCustomization['namingConvention'];
  };
};

const adaptCustomizations = (
  customization: GraphQLFieldCustomization | undefined
): CustomizationPayloadSlice | undefined => {
  if (!customization) {
    return;
  }

  const rootFields = customization?.rootFields
    ? { root_fields: customization.rootFields }
    : {};

  const typeNames = customization?.typeNames
    ? { type_names: customization.typeNames }
    : {};

  return {
    customization: {
      ...rootFields,
      ...typeNames,
      naming_convention: customization?.namingConvention,
    },
  };
};

export const addSource = (
  driver: Driver,
  payload: {
    name: string;
    dbUrl: string | { from_env: string };
    connection_parameters?: ConnectionParams;
    connection_pool_settings?: ConnectionPoolSettings;
    replace_configuration?: boolean;
    bigQuery: {
      projectId: string;
      datasets: string;
      global_select_limit: number;
    };
    sslConfiguration?: SSLConfigOptions;
    preparedStatements?: boolean;
    isolationLevel?: IsolationLevelOptions;
    customization?: GraphQLFieldCustomization;
  },
  // supported only for PG sources at the moment
  replicas?: Omit<
    SourceConnectionInfo,
    | 'connection_string'
    | 'use_prepared_statements'
    | 'ssl_configuration'
    | 'isolation_level'
  >[]
) => {
  const replace_configuration = payload.replace_configuration ?? false;
  const adaptedCustomizations = adaptCustomizations(payload?.customization);

  if (driver === 'mssql') {
    return {
      type: 'mssql_add_source',
      args: {
        name: payload.name,
        configuration: {
          connection_info: {
            connection_string: payload.dbUrl,
            pool_settings: payload.connection_pool_settings,
          },
          read_replicas: replicas?.length ? replicas : null,
        },
        replace_configuration,
        ...adaptedCustomizations,
      },
    };
  }

  if (driver === 'bigquery') {
    const service_account =
      typeof payload.dbUrl === 'string'
        ? JSON.parse(payload.dbUrl)
        : payload.dbUrl;
    return {
      type: 'bigquery_add_source',
      args: {
        name: payload.name,
        configuration: {
          service_account,
          global_select_limit: payload.bigQuery.global_select_limit,
          project_id: payload.bigQuery.projectId,
          datasets: payload.bigQuery.datasets.split(',').map(d => d.trim()),
        },
        replace_configuration,
        ...adaptedCustomizations,
      },
    };
  }

  return {
    type: `${driver === 'postgres' ? 'pg' : 'citus'}_add_source`,
    args: {
      name: payload.name,
      configuration: {
        connection_info: {
          database_url: payload.connection_parameters
            ? { connection_parameters: payload.connection_parameters }
            : payload.dbUrl,
          use_prepared_statements: payload.preparedStatements,
          isolation_level: payload.isolationLevel,
          pool_settings: payload.connection_pool_settings,
          ssl_configuration: payload.sslConfiguration,
        },
        read_replicas: replicas?.length ? replicas : null,
      },
      replace_configuration,
      ...adaptedCustomizations,
    },
  };
};

export const removeSource = (driver: Driver, name: string) => {
  let prefix = '';
  switch (driver) {
    case sourceNames.mssql:
      prefix = 'mssql_';
      break;
    case sourceNames.bigquery:
      prefix = 'bigquery_';
      break;
    case sourceNames.citus:
      prefix = 'citus_';
      break;
    default:
      prefix = 'pg_';
  }

  return {
    type: `${prefix}drop_source`,
    args: {
      name,
    },
  };
};

export const reloadSource = (name: string) => {
  return {
    type: 'reload_metadata',
    args: {
      reload_sources: [name],
    },
  };
};
