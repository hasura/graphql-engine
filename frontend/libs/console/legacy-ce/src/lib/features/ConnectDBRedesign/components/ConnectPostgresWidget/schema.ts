import { z } from 'zod';
import { graphQLCustomizationSchema } from '../GraphQLCustomization/schema';
import { Source } from '../../../hasura-metadata-types';
import { adaptPostgresConnection } from './utils/adaptResponse';

const numberSchema = z.preprocess(
  val => parseInt(val as string, 10),
  z.union([z.number().min(0), z.nan()])
);

export const poolSettingsSchema = z
  .object({
    totalMaxConnections: numberSchema.optional(),
    idleTimeout: numberSchema.optional(),
    retries: numberSchema.optional(),
    poolTimeout: numberSchema.optional(),
    connectionLifetime: numberSchema.optional(),
  })
  .optional();

export const databaseUrlSchema = z.discriminatedUnion('connectionType', [
  z.object({
    connectionType: z.literal('databaseUrl'),
    url: z.string().min(1, 'URL cannot be empty'),
  }),
  z.object({
    connectionType: z.literal('envVar'),
    envVar: z.string().min(1, 'Env variable cannot be empty'),
  }),
  z.object({
    connectionType: z.literal('dynamicFromFile'),
    dynamicFromFile: z.string().min(1, 'File path cannot be empty'),
  }),
  z.object({
    connectionType: z.literal('connectionParams'),
    username: z.string().min(1, 'Cannot be empty'),
    password: z.string().optional(),
    database: z.string().min(1, 'Cannot be empty'),
    host: z.string().min(1, 'Cannot be empty'),
    port: numberSchema,
  }),
]);

export const connectionInfoSchema = z.object({
  databaseUrl: databaseUrlSchema,
  poolSettings: poolSettingsSchema,
  usePreparedStatements: z.boolean().optional(),
  isolationLevel: z
    .union([
      z.literal('read-committed'),
      z.literal('repeatable-read'),
      z.literal('serializable'),
    ])
    .optional(),
  sslSettings: z
    .object({
      sslMode: z.string().optional(),
      sslRootCert: z.string().optional(),
      sslCert: z.string().optional(),
      sslKey: z.string().optional(),
      sslPassword: z.string().optional(),
    })
    .optional(),
});

export type ConnectionInfoSchema = z.infer<typeof connectionInfoSchema>;

export const schema = z.object({
  name: z.string().min(1, 'Database display name is a required field'),
  configuration: z.object({
    connectionInfo: connectionInfoSchema,
    readReplicas: z.array(connectionInfoSchema).optional(),
    extensionSchema: z.string().optional(),
  }),
  customization: graphQLCustomizationSchema.optional(),
});

export const getDefaultValues = (
  metadataSource?: Source
): PostgresConnectionSchema => {
  // if there is no exisiting connection, then return this template as default
  if (!metadataSource)
    return {
      name: '',
      configuration: {
        connectionInfo: {
          databaseUrl: {
            connectionType: 'envVar',
            envVar: '',
          },
        },
      },
    };

  return adaptPostgresConnection(metadataSource);
};

export type PostgresConnectionSchema = z.infer<typeof schema>;
export type PostgresConnectionInfoSchema = z.infer<typeof connectionInfoSchema>;
