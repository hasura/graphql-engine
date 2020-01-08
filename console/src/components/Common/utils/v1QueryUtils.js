export const getRunSqlQuery = (sql, shouldCascade, readOnly) => {
  const sqlWithSemicolon = sql[sql.length - 1] !== ';' ? sql + ';' : sql;
  return {
    type: 'run_sql',
    args: {
      sql: sqlWithSemicolon,
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

export const getSetTableEnumQUery = (tableDef, isEnum) => {
  return {
    type: 'set_table_is_enum',
    args: {
      table: tableDef,
      is_enum: isEnum,
    },
  };
};

export const getTrackTableQuery = tableDef => {
  return {
    type: 'add_existing_table_or_view',
    args: tableDef,
  };
};

export const getUntrackTableQuery = tableDef => {
  return {
    type: 'untrack_table',
    args: {
      table: tableDef,
    },
  };
};
