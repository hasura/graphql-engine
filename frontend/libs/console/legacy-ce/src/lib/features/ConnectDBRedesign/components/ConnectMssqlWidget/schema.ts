import { z } from 'zod';
import { graphQLCustomizationSchema } from '../GraphQLCustomization/schema';
import { Source } from '../../../hasura-metadata-types';
import { adaptMssqlConnection } from './utils/adaptResponse';

export const connectionInfoSchema = z.object({
  connectionString: z.discriminatedUnion('connectionType', [
    z.object({
      connectionType: z.literal('databaseUrl'),
      url: z.string().min(1, 'URL cannot be empty'),
    }),
    z.object({
      connectionType: z.literal('envVar'),
      envVar: z.string().min(1, 'Env variable cannot be empty'),
    }),
  ]),
  poolSettings: z
    .object({
      totalMaxConnections: z.number().min(0).optional(),
      idleTimeout: z.number().min(0).optional(),
    })
    .optional(),
});

export type ConnectionInfoSchema = z.infer<typeof connectionInfoSchema>;

export const schema = z.object({
  name: z.string().min(1, 'Database display name is a required field'),
  configuration: z.object({
    connectionInfo: connectionInfoSchema,
    readReplicas: z.array(connectionInfoSchema).optional(),
  }),
  customization: graphQLCustomizationSchema.optional(),
});

export const getDefaultValues = (
  metadataSource?: Source
): MssqlConnectionSchema => {
  // if there is no exisiting connection, then return this template as default
  if (!metadataSource)
    return {
      name: '',
      configuration: {
        connectionInfo: {
          connectionString: {
            connectionType: 'envVar',
            envVar: '',
          },
        },
      },
    };

  return adaptMssqlConnection(metadataSource);
};

export type MssqlConnectionSchema = z.infer<typeof schema>;
export type MssqlConnectionInfoSchema = z.infer<typeof connectionInfoSchema>;
