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

export const getSetCustomRootFieldsQuery = (
  tableDef,
  rootFields,
  customColumnNames
) => {
  return {
    type: 'set_table_custom_fields',
    version: 2,
    args: {
      table: tableDef,
      custom_root_fields: rootFields,
      custom_column_names: customColumnNames,
    },
  };
};

export const getRunSqlQuery = (sql, shouldCascade) => {
  return {
    type: 'run_sql',
    args: {
      sql,
      cascade: !!shouldCascade,
    },
  };
};
