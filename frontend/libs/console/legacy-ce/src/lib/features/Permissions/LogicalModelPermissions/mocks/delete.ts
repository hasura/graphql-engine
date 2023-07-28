const payload = {
  type: 'bulk',
  args: [
    {
      type: 'pg_drop_logical_model_select_permission',
      args: {
        name: 'LogicalModel',
        role: 'editor',
        source: 'Postgres',
      },
    },
  ],
  resource_version: 222,
};

const response = {
  message: 'success',
};

export default {
  payload,
  response,
};
