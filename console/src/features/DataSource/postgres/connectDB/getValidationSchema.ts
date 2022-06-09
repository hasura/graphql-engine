import { z } from 'zod';

const connection_types = {
  env_var: z.object({
    type: z.literal('env_var'),
    details: z.object({
      env_var: z.string().min(1, 'Please provide a environment variable'),
    }),
  }),
  connection_parameters: z.object({
    type: z.literal('connection_parameters'),
    details: z.object({
      host: z.string().min(1, 'Host is a required Field'),
      port: z
        .string()
        .min(1, 'Port is a required Field!')
        .transform(x => parseInt(x, 10)),
      username: z.string().min(1, 'Username is a required Field'),
      password: z.string().min(1, 'Password is a required Field'),
      db_name: z.string().min(1, 'Database Name is a required Field'),
    }),
  }),
  url: z.object({
    type: z.literal('value'),
    details: z.object({
      database_url: z.string().min(1, 'Database URL is a required Field'),
    }),
  }),
};

const database_url = z.discriminatedUnion('type', [
  connection_types.connection_parameters,
  connection_types.url,
  connection_types.env_var,
]);

const connection_info = z.object({
  database_url,
  pool_settings: z
    .object({
      max_connections: z.string().transform(x => parseInt(x, 10)),
      idle_timeout: z.string().transform(x => parseInt(x, 10)),
      retries: z.string().transform(x => parseInt(x, 10)),
      pool_timeout: z.string().transform(x => parseInt(x, 10)),
      connection_lifetime: z.string().transform(x => parseInt(x, 10)),
    })
    .optional(),
  isolation_level: z.string().transform(x => {
    if (!x) return 'read-commited';
    return x;
  }),
  prepared_statements: z.preprocess(x => {
    if (!x) return false;
    return true;
  }, z.boolean()),
});

const schema = z.object({
  driver: z.literal('postgres'),

  name: z.string().min(1, 'Name is a required field!'),
  configuration: z.object({
    connection_info,
    read_replicas: z.array(connection_info).optional(),
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
