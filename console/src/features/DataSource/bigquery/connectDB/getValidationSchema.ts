import { z } from 'zod';

const service_account = z.discriminatedUnion('type', [
  z.object({
    type: z.literal('env_var'),
    details: z.object({
      value: z.string().min(1, 'Environment variable name cannot be empty!'),
    }),
  }),
  z.object({
    type: z.literal('json'),
    details: z.object({
      value: z.string().min(1, 'Service Account Key is required!'),
    }),
  }),
]);

const schema = z.object({
  driver: z.literal('bigquery'),

  name: z.string().min(1, 'Name is a required field!'),
  configuration: z.object({
    service_account,
    project_id: z.string().min(1, 'Project ID cannot be empty!'),
    datasets: z.string().min(1, 'Datasets cannot be empty'),
  }),
  replace_configuration: z.preprocess(x => {
    if (!x) return false;
    return true;
  }, z.boolean()),
  customization: z
    .object({
      root_fields: z.object({
        namespace: z.string().optional(),
        prefix: z.string().optional(),
        suffix: z.string().optional(),
      }),
      type_names: z.object({
        prefix: z.string().optional(),
        suffix: z.string().optional(),
      }),
    })
    .optional(),
});

export const getValidationSchema = async () => {
  return schema;
};
