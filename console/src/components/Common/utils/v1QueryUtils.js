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
