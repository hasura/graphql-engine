const name = {
  label: 'Name',
  key: 'key',
  type: 'text',
};

const pool_settings = {
  key: 'pool_settings',
  label: 'Connection Settings',
  fields: [
    {
      key: 'max_connections',
      label: 'Max Connections',
      type: 'number',
      tooltip: 'Maximum number of connections to be kept in the pool',
      placeholder: 50,
    },
    {
      key: 'idle_timeout',
      label: 'Idle Timeout',
      type: 'number',
      tooltip: 'The idle timeout (in seconds) per connection',
      placeholder: 180,
    },
    {
      key: 'retries',
      label: 'Retries',
      type: 'number',
      tooltip: 'Number of retries to perform',
      placeholder: 1,
    },
    {
      key: 'pool_timeout',
      label: 'Pool Timeout',
      type: 'number',
      tooltip:
        'Maximum time (in seconds) to wait while acquiring a Postgres connection from the pool',
      placeholder: 360,
    },
    {
      key: 'connection_lifetime',
      label: 'Connection Lifetime',
      type: 'number',
      tooltip:
        'Time (in seconds) from connection creation after which the connection should be destroyed and a new one created. A value of 0 indicates we should never destroy an active connection. If 0 is passed, memory from large query results may not be reclaimed.',
      placeholder: 600,
    },
  ],
};

const database_url = {
  key: 'database_url',
  label: '',
  type: 'radio-group-with-inputs',
  options: [
    {
      key: 'env_var',
      label: 'Enviroment Variable',
      fields: [
        {
          label: 'Env var',
          key: 'env_var',
          type: 'text',
          placeholder: 'HASURA_DB_URL_FROM_ENV',
        },
      ],
    },
    {
      key: 'database_url',
      label: 'Database URL',
      fields: [
        {
          label: 'URL',
          key: 'database_url',
          type: 'text',
          placeholder: 'postgresql://username:password@hostname:5432/database',
        },
      ],
      expand_keys: true,
    },
    {
      key: 'connection_parameters',
      label: 'Connection Parameters',
      fields: [
        {
          label: 'Username',
          key: 'username',
          type: 'text',
          placeholder: 'postgres_user',
        },
        {
          label: 'Password',
          key: 'password',
          type: 'password',
          placeholder: 'postgrespassword',
        },
        {
          label: 'Database Name',
          key: 'database',
          type: 'text',
          placeholder: 'postgres',
        },
        {
          label: 'Host',
          key: 'host',
          type: 'text',
          placeholder: 'localhost',
        },
        {
          label: 'Port',
          key: 'port',
          type: 'number',
          placeholder: '5432',
        },
      ],
      expand_keys: true,
    },
  ],
};

const connection_info = {
  key: 'connection_info',
  label: 'Connection Details',
  fields: [
    database_url,
    pool_settings,
    {
      key: 'isolation_level',
      label: 'Isolation Level',
      type: 'select',
      tooltip:
        'The transaction isolation level in which the queries made to the source will be run',
      options: ['read-commited', 'repeatable-read', 'serializable'],
    },
    {
      key: 'prepared_statements',
      label: 'Use Prepared Statements',
      type: 'boolean',
      tooltip: 'Prepared statements are disabled by default',
    },
  ],
};

const read_replicas = {
  key: 'read_replicas',
  label: 'Read Replicas',
  type: 'array',
  fields: [connection_info],
};

const configuration = {
  key: 'configuration',
  label: 'Connect Database Via',
  fields: [connection_info, read_replicas],
};

const customization = {
  key: 'customization',
  label: 'GraphQL Customization',
  fields: [
    {
      label: 'Namespace',
      key: 'root_fields.namespace',
      type: 'text',
    },
    {
      label: 'Prefix',
      key: 'root_fields.prefix',
      type: 'text',
    },
    {
      label: 'Suffix',
      key: 'root_fields.suffix',
      type: 'text',
    },
    {
      label: 'Prefix',
      key: 'type_names.prefix',
      type: 'text',
    },
    {
      label: 'Suffix',
      key: 'type_names.suffix',
      type: 'text',
    },
  ],
};

export const getUISchema = async () => {
  return [name, configuration, customization];
};
