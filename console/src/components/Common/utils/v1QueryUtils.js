export const getCreatePermissionQuery = (
  action,
  tableDef,
  role,
  permission
) => {
  return {
    type: 'create_' + action + '_permission',
    args: {
      table: tableDef,
      role: role,
      permission: permission,
    },
  };
};

export const getDropPermissionQuery = (action, tableDef, role) => {
  return {
    type: 'drop_' + action + '_permission',
    args: {
      table: tableDef,
      role: role,
    },
  };
};

export const generateSetCustomTypesQuery = customTypes => {
  return {
    type: 'set_custom_types',
    args: customTypes,
  };
};

export const generateCreateActionQuery = (name, definition) => {
  return {
    type: 'create_action',
    args: {
      name,
      definition,
    },
  };
};

export const generateDropActionQuery = name => {
  return {
    type: 'drop_action',
    args: {
      name,
    },
  };
};

export const getFetchActionsQuery = () => {
  return {
    type: 'select',
    args: {
      table: {
        name: 'hdb_action',
        schema: 'hdb_catalog',
      },
      columns: ['*.*'],
      order_by: [{ column: 'action_name', type: 'asc' }],
    },
  };
};

export const getFetchCustomTypesQuery = () => {
  return {
    type: 'select',
    args: {
      table: {
        name: 'hdb_custom_types',
        schema: 'hdb_catalog',
      },
      columns: ['*.*'],
    },
  };
};
