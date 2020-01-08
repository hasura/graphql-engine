export const getRunSqlQuery = (sql, shouldCascade, readOnly) => {
  return {
    type: 'run_sql',
    args: {
      sql,
      cascade: !!shouldCascade,
      read_only: !!readOnly,
    },
  };
};

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

export const getAddComputedFieldQuery = (
  tableDef,
  computedFieldName,
  definition,
  comment
) => {
  return {
    type: 'add_computed_field',
    args: {
      table: tableDef,
      name: computedFieldName,
      definition: {
        ...definition,
      },
      comment: comment,
    },
  };
};

export const getDropComputedFieldQuery = (tableDef, computedFieldName) => {
  return {
    type: 'drop_computed_field',
    args: {
      table: tableDef,
      name: computedFieldName,
    },
  };
};
