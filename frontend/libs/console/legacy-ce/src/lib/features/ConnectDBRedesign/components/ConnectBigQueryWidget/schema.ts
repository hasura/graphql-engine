import { z } from 'zod';
import { graphQLCustomizationSchema } from '../GraphQLCustomization/schema';
import { Source } from '../../../hasura-metadata-types';
import { adaptPostgresConnection } from './utils/adaptResponse';

export const schema = z.object({
  name: z.string().min(1, 'Database display name is a required field'),
  configuration: z.object({
    serviceAccount: z.discriminatedUnion('type', [
      z.object({
        type: z.literal('envVar'),
        envVar: z.string().min(1, 'Env variable cannot be empty'),
      }),
      z.object({
        type: z.literal('serviceAccountKey'),
        value: z.any(),
      }),
    ]),
    projectId: z.discriminatedUnion('type', [
      z.object({
        type: z.literal('envVar'),
        envVar: z.string().min(1, 'Env variable cannot be empty'),
      }),
      z.object({
        type: z.literal('value'),
        value: z.string().min(1, 'Project ID cannot be empty'),
      }),
    ]),
    datasets: z.discriminatedUnion('type', [
      z.object({
        type: z.literal('envVar'),
        envVar: z.string().min(1, 'Env variable cannot be empty'),
      }),
      z.object({
        type: z.literal('value'),
        value: z.string().min(1, 'Service account key cannot be empty'),
      }),
    ]),
  }),
  customization: graphQLCustomizationSchema.optional(),
});

export const getDefaultValues = (
  metadataSource?: Source
): BigQueryConnectionSchema => {
  // if there is no exisiting connection, then return this template as default
  if (!metadataSource)
    return {
      name: '',
      configuration: {
        serviceAccount: {
          type: 'envVar',
          envVar: '',
        },
        projectId: {
          type: 'envVar',
          envVar: '',
        },
        datasets: {
          type: 'envVar',
          envVar: '',
        },
      },
    };

  return adaptPostgresConnection(metadataSource);
};

export type BigQueryConnectionSchema = z.infer<typeof schema>;
