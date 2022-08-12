import { Property } from '../../types';

export const getDatabaseConfiguration = async () => {
  const {
    configSchema,
    otherSchemas,
  }: { configSchema: Property; otherSchemas: Record<string, Property> } = {
    configSchema: {
      type: 'object',
      nullable: false,
      description: 'Configuration',
      properties: {
        connection_info: { $ref: '#/otherSchemas/ConnectionInfo' },
      },
    },
    otherSchemas: {
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
        },
      },
    },
  };

  return { configSchema, otherSchemas };
};
