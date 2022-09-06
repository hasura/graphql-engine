import { Property } from '../../types';

export const getDatabaseConfiguration = async () => {
  const {
    configSchema,
    otherSchemas,
  }: { configSchema: Property; otherSchemas: Record<string, Property> } = {
    configSchema: {
      type: 'object',
      nullable: false,
      description: 'Configuration (complex object example)',
      properties: {
        connection_info: { $ref: '#/otherSchemas/ConnectionInfo' },
      },
    },
    otherSchemas: {
      TableName: {
        nullable: false,
        type: 'string',
      },
      PoolSettings: {
        type: 'object',
        description: 'Pool Settings',
        nullable: true,
        properties: {
          max_connections: {
            description: 'Max connections',
            type: 'number',
            nullable: true,
          },
          idle_timeout: {
            description: 'Idle Timeout',
            type: 'number',
            nullable: true,
          },
          retries: {
            description: 'Retries',
            type: 'number',
            nullable: true,
          },
          pool_timeout: {
            description: 'Pool Timeout',
            type: 'number',
            nullable: true,
          },
          connection_lifetime: {
            description: 'Connection Lifetime',
            type: 'number',
            nullable: true,
          },
        },
      },
      DatabaseURL: {
        description: 'Database URL',
        type: 'string',
        nullable: false,
      },
      EnvironmentVariable: {
        description: 'Enviroment Variable',
        type: 'object',
        nullable: false,
        properties: {
          from_env: {
            description: 'Enviroment Variable',
            type: 'string',
            nullable: false,
          },
        },
      },
      ConnectionInfo: {
        description: 'Connection details',
        type: 'object',
        nullable: false,
        properties: {
          database_url: {
            description: 'Connect DB via',
            oneOf: [
              { $ref: '#/otherSchemas/DatabaseURL' },
              {
                type: 'object',
                nullable: false,
                description: 'Connection Parameters',
                properties: {
                  username: {
                    description: 'Username',
                    type: 'string',
                    nullable: false,
                  },
                  password: {
                    description: 'Password',
                    type: 'string',
                    nullable: false,
                  },
                  host: {
                    description: 'Host',
                    type: 'string',
                    nullable: false,
                  },
                  database: {
                    description: 'Database',
                    type: 'string',
                    nullable: false,
                  },
                  port: {
                    description: 'Port',
                    type: 'number',
                    nullable: false,
                  },
                },
              },
              { $ref: '#/otherSchemas/EnvironmentVariable' },
            ],
          },
          pool_settings: { $ref: '#/otherSchemas/PoolSettings' },
          use_prepared_statements: {
            description: 'Use Prepared Statements',
            type: 'boolean',
            nullable: true,
          },
          isolation_level: {
            description: 'Isolation Level',
            type: 'string',
            nullable: true,
            enum: ['read-committed', 'repeatable-read', 'serializable'],
          },
          list_of_table_names: {
            type: 'array',
            description: 'Tables List (This is an array of strings input)',
            items: {
              type: 'string',
            },
          },
          list_of_ports: {
            type: 'array',
            description: 'Ports List (This is an array of number input)',
            items: {
              type: 'number',
            },
          },
          ref_array_input: {
            description:
              'List of tables (This is an array of strings input but the definition is a ref)',
            type: 'array',
            items: {
              $ref: '#/otherSchemas/TableName',
            },
            nullable: true,
          },
          list_of_connections: {
            description:
              'List of multiple connections (This is a array of objects)',
            type: 'array',
            items: {
              type: 'object',
              properties: {
                username: {
                  description: 'Username',
                  type: 'string',
                  nullable: false,
                },
                password: {
                  description: 'Password',
                  type: 'string',
                  nullable: false,
                },
                host: {
                  description: 'Host',
                  type: 'string',
                  nullable: false,
                },
                database: {
                  description: 'Database',
                  type: 'string',
                  nullable: false,
                },
                port: {
                  description: 'Port',
                  type: 'number',
                  nullable: false,
                },
              },
            },
            nullable: true,
          },
          DEBUG: {
            description: 'For debugging (Free form JSON field example)',
            type: 'object',
            additionalProperties: true,
            nullable: true,
          },
          DEBUG_array: {
            description:
              'For debugging (Free form array of JSON field example)',
            type: 'array',
            items: {
              type: 'object',
              additionalProperties: true,
              nullable: true,
            },
            nullable: true,
          },
          ref_array_object_input: {
            description:
              'List of multiple connections (This is a array of objects, object is a ref)',
            type: 'array',
            items: {
              $ref: '#/otherSchemas/ConnectionInfo',
            },
            nullable: true,
          },
        },
      },
    },
  };

  return { configSchema, otherSchemas };
};
