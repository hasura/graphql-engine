import {
  Action,
  AllowList,
  CronTrigger,
  CustomFunction,
  CustomTypes,
  QueryCollectionEntry,
  RemoteSchema,
  TableEntry,
} from "./HasuraMetadataV2"

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#fromenv
 */
export interface FromEnv {
  /** Name of the environment variable */
  from_env: string
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgconfiguration
 */
export interface PGConfiguration {
  /** Connection parameters for the source */
  connection_info: PGSourceConnectionInfo
  /** Optional list of read replica configuration (supported only in cloud/enterprise versions) */
  read_replicas?: PGSourceConnectionInfo[]
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#mssqlsourceconnectioninfo
 */
export interface MsSQLConfiguration {
  /** Connection parameters for the source */
  connection_info: MsSQLSourceConnectionInfo
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#bigqueryconfiguration
 */
export interface BigQueryConfiguration {
  /** Service account for BigQuery database */
  service_account: string | Record<string, any> | FromEnv
  /** Project Id for BigQuery database */
  project_id: string | FromEnv
  /** List of BigQuery datasets */
  datasets: string[] | FromEnv
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgsourceconnectioninfo
 */
export interface PGSourceConnectionInfo {
  /** The database connection URL as a string, as an environment variable, or as connection parameters. */
  database_url: string | FromEnv | PGConnectionParameters
  /** Connection pool settings */
  pool_settings?: PGPoolSettings

  /** If set to true the server prepares statement before executing on the source database (default: false). For more details, refer to the Postgres docs */
  use_prepared_statements?: boolean

  /** The transaction isolation level in which the queries made to the source will be run with (default: read-committed). */
  isolation_level?: "read-committed" | "repeatable-read" | "serializable"

  /** The client SSL certificate settings for the database (Only available in Cloud). */
  ssl_configuration?: PGCertSettings
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#mssqlsourceconnectioninfo
 */
export interface MsSQLSourceConnectionInfo {
  /** The database connection string, or as an environment variable */
  connection_string: string | FromEnv
  /** Connection pool settings */
  pool_settings?: MsSQLPoolSettings
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgconnectionparameters
 */
export interface PGConnectionParameters {
  /**The Postgres user to be connected */
  username: string
  /** The Postgres user’s password */
  password?: string
  /** The database name */
  database: string
  /** The name of the host to connect to */
  host: string
  /** The port number to connect with, at the server host */
  port: number
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgpoolsettings
 */
export interface PGPoolSettings {
  /** Maximum number of connections to be kept in the pool (default: 50) */
  max_connections?: number
  /** The idle timeout (in seconds) per connection (default: 180) */
  idle_timeout?: number
  /**	Number of retries to perform (default: 1) */
  retries?: number
  /** Maximum time to wait while acquiring a Postgres connection from the pool, in seconds (default: forever) */
  pool_timeout?: number
  /** Time from connection creation after which the connection should be destroyed and a new one created. A value of 0 indicates we should never destroy an active connection. If 0 is passed, memory from large query results may not be reclaimed. (default: 600 sec) */
  connection_lifetime?: number
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgcertsettings
 */
export interface PGCertSettings {
  /** The SSL connection mode. See the libpq ssl support docs <https://www.postgresql.org/docs/9.1/libpq-ssl.html> for more details. */
  sslmode: string
  /** Environment variable which stores trusted certificate authorities. */
  sslrootcert: FromEnv
  /** Environment variable which stores the client certificate. */
  sslcert: FromEnv
  /** Environment variable which stores the client private key. */
  sslkey: FromEnv
  /** Password in the case where the sslkey is encrypted. */
  sslpassword?: string | FromEnv
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#mssqlpoolsettings
 */
export interface MsSQLPoolSettings {
  /** Maximum number of connections to be kept in the pool (default: 50) */
  max_connections?: number
  /** The idle timeout (in seconds) per connection (default: 180) */
  idle_timeout?: number
}

export enum BackendKind {
  POSTGRES = "postgres",
  MSSQL = "mssql",
  CITUS = "citus",
  BIGQUERY = "bigquery",
}

interface BaseSource {
  name: string
  tables: TableEntry[]
  functions?: CustomFunction[]
}

interface PGSource extends BaseSource {
  kind: BackendKind.POSTGRES | BackendKind.CITUS
  configuration: PGConfiguration
}

interface MsSQLSource extends BaseSource {
  kind: BackendKind.MSSQL
  configuration: MsSQLConfiguration
}

interface BigQuerySource extends BaseSource {
  kind: BackendKind.BIGQUERY
  configuration: BigQueryConfiguration
}

export type Source = PGSource | MsSQLSource | BigQuerySource

export interface APILimits {
  depth_limit?: DepthLimit;
  disabled: boolean;
  rate_limit?: RateLimit;
  node_limit?: NodeLimit;
}

export interface DepthLimit {
  global: number;
  per_role: {
    [role: string]: number;
  };
}

export interface RateLimit {
  global: RateLimitRule;
  per_role: {
    [role: string]: RateLimitRule;
  };
}

export interface RateLimitRule {
  unique_params: null | 'IP' | string[];
  max_reqs_per_min: number;
}

export interface NodeLimit {
  global: number;
  per_role: {
    [role: string]: number;
  };
}

export interface RestEndpoint {
  name: string;
  url: string;
  methods: ('POST' | 'PUT' | 'PATCH')[];
  definition: RestEndpointDefinition;
  comment?: string;
}

export interface RestEndpointDefinition {
  query: {
    query_name: string;
    collection_name: string;
  };
}

export interface InheritedRole {
  role_name: string;
  role_set: string[];
}

// quicktype code generator can't handle Typescript's "Omit<T>" type
export interface HasuraMetadataV3 {
  version: 3
  sources: Source[]
  actions?: Action[]
  custom_types?: CustomTypes
  remote_schemas?: RemoteSchema[]
  query_collections?: QueryCollectionEntry[]
  allowlist?: AllowList[]
  cron_triggers?: CronTrigger[]
  api_limits?: APILimits
  rest_endpoints: RestEndpoint[]
  inherited_roles?: InheritedRole[]
}
