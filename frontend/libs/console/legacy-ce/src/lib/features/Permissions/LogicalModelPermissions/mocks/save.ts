const payload = {
  type: 'bulk',
  args: [
    {
      type: 'pg_drop_logical_model_select_permission',
      args: {
        permission: {},
        name: 'LogicalModel',
        role: 'editor',
        source: 'Postgres',
      },
    },
    {
      type: 'pg_create_logical_model_select_permission',
      args: {
        name: 'LogicalModel',
        role: 'editor',
        permission: {
          filter: {},
          columns: [],
        },
        source: 'Postgres',
      },
    },
  ],
  resource_version: 221,
};

const response = [
  {
    message: 'success',
  },
  {
    message: 'success',
  },
];

export default { payload, response };
