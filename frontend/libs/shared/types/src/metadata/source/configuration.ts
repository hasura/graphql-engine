export type FromEnv = { from_env: string };
type ValidJson = Record<string, any>;

/**
 * Docs for type: https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgsourceconnectioninfo
 */

export type SSLModeOptions = 'verify-ca' | 'verify-full' | 'disable';

export type IsolationLevelOptions =
  'read-committed' | 'repeatable-read' | 'serializable';

export type PostgresConnectionObjectParams = {
  username: string;
  password?: string;
  database: string;
  host: string;
  port: number;
};

export type PostgresConnectionDatabaseURLConfig =
  | string
  | FromEnv
  | { dynamic_from_file: string }
  | PostgresConnectionObjectParams;

interface PostgresConnectionInfo {
  database_url: PostgresConnectionDatabaseURLConfig;
  pool_settings?: {
    max_connections?: number;
    total_max_connections?: number;
    idle_timeout?: number;
    retries?: number;
    pool_timeout?: number;
    connection_lifetime?: number;
  };
  use_prepared_statements?: boolean;
  /**
   * The transaction isolation level in which the queries made to the source will be run with (default: read-committed).
   */
  isolation_level?: IsolationLevelOptions;
  ssl_configuration?: PostgresSSLConfiguration;
}

export interface PostgresSSLConfiguration {
  sslmode: SSLModeOptions;
  sslrootcert: FromEnv;
  sslcert: FromEnv;
  sslkey: FromEnv;
  sslpassword: FromEnv;
}

export type PostgresConnectionSet = {
  name: string;
  connection_info: PostgresConnectionInfo;
};

export interface PostgresConfiguration {
  connection_info: PostgresConnectionInfo;
  /**
   * Kriti template to resolve connection info at runtime
   */
  connection_template?: {
    template: string;
  };
  /**
   * List of connection sets to use in connection template
   */
  connection_set?: PostgresConnectionSet[];

  /**
   * Optional list of read replica configuration (supported only in cloud/enterprise versions)
   */
  read_replicas?: PostgresConfiguration['connection_info'][];
  /**
   * Name of the schema where the graphql-engine will install database extensions (default: public)
   */
  extensions_schema?: any;
}

export interface MssqlConfiguration {
  connection_info: {
    connection_string: string | FromEnv;
    pool_settings?: {
      total_max_connections?: number | null;
      idle_timeout?: number;
    };
  };
  read_replicas?: MssqlConfiguration['connection_info'][];
}

export interface BigQueryConfiguration {
  service_account: ValidJson | FromEnv;
  project_id: string | FromEnv;
  datasets: string[] | FromEnv;
  global_select_limit?: number;
}

export type CitusConfiguration = PostgresConfiguration;
