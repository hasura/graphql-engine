import {
  defaultPermissionsState,
  defaultQueryPermissions,
  defaultInsertSetState,
} from '../DataState';
import { getEdForm, getIngForm } from '../utils';
import { makeMigrationCall } from '../DataActions';

export const PERM_OPEN_EDIT = 'ModifyTable/PERM_OPEN_EDIT';
export const PERM_SET_FILTER = 'ModifyTable/PERM_SET_FILTER';
export const PERM_SET_FILTER_SAME_AS = 'ModifyTable/PERM_SET_FILTER_SAME_AS';
export const PERM_TOGGLE_COLUMN = 'ModifyTable/PERM_TOGGLE_COLUMN';
export const PERM_TOGGLE_ALL_COLUMNS = 'ModifyTable/PERM_TOGGLE_ALL_COLUMNS';
export const PERM_ALLOW_ALL = 'ModifyTable/PERM_ALLOW_ALL';
export const PERM_TOGGLE_ENABLE_LIMIT = 'ModifyTable/PERM_TOGGLE_ENABLE_LIMIT';
export const PERM_TOGGLE_MODIFY_LIMIT = 'ModifyTable/PERM_TOGGLE_MODIFY_LIMIT';
export const PERM_TOGGLE_ALLOW_UPSERT = 'ModifyTable/PERM_TOGGLE_ALLOW_UPSERT';
export const PERM_TOGGLE_ALLOW_AGGREGATION =
  'ModifyTable/PERM_TOGGLE_ALLOW_AGGREGATION';
export const PERM_CUSTOM_CHECKED = 'ModifyTable/PERM_CUSTOM_CHECKED';

export const PERM_REMOVE_ACCESS = 'ModifyTable/PERM_REMOVE_ACCESS';
export const PERM_SAVE_PERMISSIONS = 'ModifyTable/PERM_SAVE_PERMISSIONS';
export const PERM_CLOSE_EDIT = 'ModifyTable/PERM_CLOSE_EDIT';
export const PERM_SET_ROLE_NAME = 'ModifyTable/PERM_SET_ROLE_NAME';
export const PERM_SELECT_BULK = 'ModifyTable/PERM_SELECT_BULK';
export const PERM_DESELECT_BULK = 'ModifyTable/PERM_DESELECT_BULK';
export const PERM_RESET_BULK_SELECT = 'ModifyTable/PERM_RESET_BULK_SELECT';
export const PERM_RESET_BULK_SAME_SELECT =
  'ModifyTable/PERM_RESET_BULK_SAME_SELECT';
export const PERM_SAME_APPLY_BULK = 'ModifyTable/PERM_SAME_APPLY_BULK';
export const PERM_DESELECT_SAME_APPLY_BULK =
  'ModifyTable/PERM_DESELECT_SAME_APPLY_BULK';

export const X_HASURA_CONST = 'x-hasura-';

/* insert set operations */
export const UPDATE_PERM_SET_KEY_VALUE =
  'ModifyTable/UPDATE_PERM_SET_KEY_VALUE';

export const CREATE_NEW_INSERT_SET_VAL =
  'ModifyTable/CREATE_NEW_INSERT_SET_VAL';

export const DELETE_INSERT_SET_VAL = 'ModifyTable/DELETE_INSERT_SET_VAL';

export const TOGGLE_PERM_INSERT_SET_OPERATION_CHECK =
  'ModifyTable/TOGGLE_PERM_INSERT_SET_OPERATION_CHECK';
export const SET_TYPE_CONFIG = 'ModifyTable/SET_TYPE_CONFIG';

/* */

const getQueriesWithPermColumns = insert => {
  const queries = ['select', 'update'];
  if (insert) {
    queries.push('insert');
  }
  return queries;
};
const permChangeTypes = {
  save: 'update',
  delete: 'delete',
};

const permOpenEdit = (
  tableSchema,
  role,
  query,
  insertPermColumnRestriction
) => ({
  type: PERM_OPEN_EDIT,
  tableSchema,
  role,
  query,
  insertPermColumnRestriction,
});
const permSetFilter = filter => ({ type: PERM_SET_FILTER, filter });
const permSetFilterSameAs = filter => ({
  type: PERM_SET_FILTER_SAME_AS,
  filter,
});
const permToggleColumn = column => ({ type: PERM_TOGGLE_COLUMN, column });
const permToggleAllColumns = allColumns => ({
  type: PERM_TOGGLE_ALL_COLUMNS,
  allColumns,
});
const permAllowAll = () => ({ type: PERM_ALLOW_ALL });
const permCloseEdit = () => ({ type: PERM_CLOSE_EDIT });
const permSetRoleName = roleName => ({
  type: PERM_SET_ROLE_NAME,
  data: roleName,
});
const _permSavePermissions = () => ({ type: PERM_SAVE_PERMISSIONS });
const _permRemoveAccess = () => ({ type: PERM_REMOVE_ACCESS });
const permToggleAllowUpsert = checked => ({
  type: PERM_TOGGLE_ALLOW_UPSERT,
  data: checked,
});
const permToggleAllowAggregation = checked => ({
  type: PERM_TOGGLE_ALLOW_AGGREGATION,
  data: checked,
});
const permToggleModifyLimit = limit => ({
  type: PERM_TOGGLE_MODIFY_LIMIT,
  data: limit,
});
const permSetBulkSelect = (isChecked, selectedRole) => {
  return dispatch => {
    if (isChecked) {
      dispatch({ type: PERM_SELECT_BULK, data: selectedRole });
    } else {
      dispatch({ type: PERM_DESELECT_BULK, data: selectedRole });
    }
  };
};
const permSetSameSelect = (isChecked, selectedRole) => {
  return dispatch => {
    if (isChecked) {
      dispatch({ type: PERM_SAME_APPLY_BULK, data: selectedRole });
    } else {
      dispatch({ type: PERM_DESELECT_SAME_APPLY_BULK, data: selectedRole });
    }
  };
};
const permCustomChecked = () => ({ type: PERM_CUSTOM_CHECKED });

const getFilterKey = query => {
  return query === 'insert' ? 'check' : 'filter';
};

const setConfigValueType = value => {
  return typeof value === 'string' &&
    value.slice(0, X_HASURA_CONST.length) === X_HASURA_CONST
    ? 'session'
    : 'static';
};

const getBasePermissionsState = (
  tableSchema,
  role,
  query,
  insertPermColumnRestriction
) => {
  const _permissions = JSON.parse(JSON.stringify(defaultPermissionsState));

  _permissions.table = tableSchema.table_name;
  _permissions.role = role;
  _permissions.query = query;

  const rolePermissions = tableSchema.permissions.find(
    p => p.role_name === role
  );
  if (rolePermissions) {
    Object.keys(rolePermissions.permissions).forEach(q => {
      let set = [];
      _permissions[q] = rolePermissions.permissions[q];
      // If the query is insert, transform set object if exists to an array
      if (q === 'insert') {
        // If set is an object
        if (insertPermColumnRestriction) {
          if (!_permissions[q].columns) {
            _permissions[q].columns = tableSchema.columns.map(
              c => c.column_name
            );
          }
        }
        if ('set' in _permissions[q]) {
          if (
            Object.keys(_permissions[q].set).length > 0 &&
            !(_permissions[q].set.length > 0)
          ) {
            Object.keys(_permissions[q].set).map(s => {
              set.push({
                key: s,
                value: _permissions[q].set[s],
              });
            });
            set.push(defaultInsertSetState);
            _permissions[q].isSetConfigChecked = true;
          } else if (
            'localSet' in _permissions[q] &&
            _permissions[q].localSet.length > 0
          ) {
            set = [..._permissions[q].localSet];
            _permissions[q].isSetConfigChecked = true;
          } else {
            set.push(defaultInsertSetState);
          }
          _permissions[q].localSet = [...set];
        } else {
          // Just to support version changes
          // If user goes from current to previous version and back
          _permissions[q].localSet = [defaultInsertSetState];
          _permissions[q].set = {};
        }
      }
    });
  } else {
    _permissions.newRole = role;
  }

  return _permissions;
};

const updatePermissionsState = (permissions, key, value) => {
  const _permissions = JSON.parse(JSON.stringify(permissions));

  const query = permissions.query;

  _permissions[query] =
    _permissions[query] ||
    JSON.parse(JSON.stringify(defaultQueryPermissions[query]));
  _permissions[query][key] = value;

  return _permissions;
};

const updateBulkSelect = (permissionsState, selectedRole, isAdd) => {
  let bulkRes = permissionsState.bulkSelect;
  if (isAdd) {
    bulkRes.push(selectedRole);
  } else {
    bulkRes = bulkRes.filter(e => e !== selectedRole);
  }
  return bulkRes;
};

const updateBulkSameSelect = (permissionsState, selectedRole, isAdd) => {
  let bulkRes = permissionsState.applySamePermissions;
  if (isAdd) {
    bulkRes.push(selectedRole);
  } else {
    bulkRes = bulkRes.filter(e => e !== selectedRole);
  }
  return bulkRes;
};

const deleteFromPermissionsState = permissions => {
  const _permissions = JSON.parse(JSON.stringify(permissions));

  const query = permissions.query;

  delete _permissions[query];

  return _permissions;
};

const toggleAllColumns = (permissions, allColumns) => {
  const currColumns = permissions ? permissions.columns : [];

  return currColumns.length === allColumns.length ? [] : allColumns;
};

const toggleColumn = (permissions, column) => {
  const currColumns = permissions ? permissions.columns : [];
  let _newColumns = currColumns;

  const columnIndex = currColumns.indexOf(column);
  if (columnIndex === -1) {
    _newColumns.push(column);
  } else {
    _newColumns.splice(columnIndex, 1);
  }

  _newColumns = _newColumns.sort();

  return _newColumns;
};

const permRemoveRole = (tableSchema, roleName) => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;

    const table = tableSchema.table_name;
    const role = roleName;

    const currRolePermissions = tableSchema.permissions.find(
      p => p.role_name === role
    );

    const permissionsUpQueries = [];
    const permissionsDownQueries = [];

    if (currRolePermissions && currRolePermissions.permissions) {
      Object.keys(currRolePermissions.permissions).forEach(type => {
        const deleteQuery = {
          type: 'drop_' + type + '_permission',
          args: {
            table: { name: table, schema: currentSchema },
            role: role,
          },
        };
        const createQuery = {
          type: 'create_' + type + '_permission',
          args: {
            table: { name: table, schema: currentSchema },
            role: role,
            permission: currRolePermissions.permissions[type],
          },
        };
        permissionsUpQueries.push(deleteQuery);
        permissionsDownQueries.push(createQuery);
      });
    }

    // Apply migration
    const migrationName =
      'remove_permission_' + role + '_' + currentSchema + '_table_' + table;

    const requestMsg = 'Removing permissions...';
    const successMsg = 'Permission removed';
    const errorMsg = 'Removing permissions failed';

    const customOnSuccess = () => {
      dispatch(_permRemoveAccess());
      // reset new role name
      dispatch(permSetRoleName(''));
      // close edit box
      dispatch(permCloseEdit());
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      permissionsUpQueries,
      permissionsDownQueries,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const permRemoveMultipleRoles = tableSchema => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const permissionsState = getState().tables.modify.permissionsState;

    const table = tableSchema.table_name;
    const roles = permissionsState.bulkSelect;

    const permissionsUpQueries = [];
    const permissionsDownQueries = [];
    const currentPermissions = tableSchema.permissions;

    roles.map(role => {
      const currentRolePermission = currentPermissions.filter(el => {
        return el.role_name === role;
      });
      Object.keys(currentRolePermission[0].permissions).forEach(type => {
        const deleteQuery = {
          type: 'drop_' + type + '_permission',
          args: {
            table: { name: table, schema: currentSchema },
            role: role,
          },
        };
        const createQuery = {
          type: 'create_' + type + '_permission',
          args: {
            table: { name: table, schema: currentSchema },
            role: role,
            permission: currentRolePermission[0].permissions[type],
          },
        };
        permissionsUpQueries.push(deleteQuery);
        permissionsDownQueries.push(createQuery);
      });
    });

    // Apply migration
    const migrationName = 'remove_roles_' + currentSchema + '_table_' + table;

    const requestMsg = 'Removing roles...';
    const successMsg = 'Roles removed';
    const errorMsg = 'Removing roles failed';

    const customOnSuccess = () => {
      // reset new role name
      dispatch(permSetRoleName(''));
      // close edit box
      dispatch(permCloseEdit());
      // reset checkbox selections
      dispatch({ type: PERM_RESET_BULK_SELECT });
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      permissionsUpQueries,
      permissionsDownQueries,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const applySamePermissionsBulk = tableSchema => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const permissionsState = getState().tables.modify.permissionsState;

    const table = tableSchema.table_name;
    const currentQueryType = permissionsState.query;
    const toBeAppliedPermission = permissionsState[currentQueryType];
    const selectedRoles = permissionsState.applySamePermissions;

    const permissionsUpQueries = [];
    const permissionsDownQueries = [];
    const currentPermissions = tableSchema.permissions;

    selectedRoles.map(role => {
      // find out if selected role has an existing permission of the same query type.
      // if so add a drop permission and then create the new permission.

      const currentRolePermission = currentPermissions.filter(el => {
        return el.role_name === role;
      });
      if (currentRolePermission[0].permissions[currentQueryType]) {
        // existing permission is there. so drop and recreate.
        const deleteQuery = {
          type: 'drop_' + currentQueryType + '_permission',
          args: {
            table: { name: table, schema: currentSchema },
            role: role,
          },
        };
        const createQuery = {
          type: 'create_' + currentQueryType + '_permission',
          args: {
            table: { name: table, schema: currentSchema },
            role: role,
            permission: currentRolePermission[0].permissions[currentQueryType],
          },
        };
        permissionsUpQueries.push(deleteQuery);
        permissionsDownQueries.push(createQuery);
      }
      // now add normal create and drop permissions
      const createQuery = {
        type: 'create_' + currentQueryType + '_permission',
        args: {
          table: { name: table, schema: currentSchema },
          role: role,
          permission: toBeAppliedPermission,
        },
      };
      const deleteQuery = {
        type: 'drop_' + currentQueryType + '_permission',
        args: {
          table: { name: table, schema: currentSchema },
          role: role,
        },
      };
      permissionsUpQueries.push(createQuery);
      permissionsDownQueries.push(deleteQuery);
    });

    // Apply migration
    const migrationName =
      'apply_same_permissions_' + currentSchema + '_table_' + table;

    const requestMsg = 'Applying Same Permissions';
    const successMsg = 'Permission Changes Applied';
    const errorMsg = 'Permisison Changes Failed';

    const customOnSuccess = () => {
      // reset new role name
      dispatch(permSetRoleName(''));
      // close edit box
      dispatch(permCloseEdit());
      // reset checkbox selections
      dispatch({ type: PERM_RESET_BULK_SAME_SELECT });
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      permissionsUpQueries,
      permissionsDownQueries,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const permChangePermissions = changeType => {
  return (dispatch, getState) => {
    const allSchemas = getState().tables.allSchemas;
    const currentSchema = getState().tables.currentSchema;
    const permissionsState = {
      ...getState().tables.modify.permissionsState,
    };
    const limitEnabled = permissionsState.limitEnabled;

    const table = permissionsState.table;
    const role = permissionsState.role;
    const query = permissionsState.query;

    const tableSchema = allSchemas.find(t => t.table_name === table);
    const currRolePermissions = tableSchema.permissions.find(
      p => p.role_name === role
    );

    const permissionsUpQueries = [];
    const permissionsDownQueries = [];
    if (query === 'select' && !limitEnabled) {
      delete permissionsState[query].limit;
    }

    if (currRolePermissions && currRolePermissions.permissions[query]) {
      const deleteQuery = {
        type: 'drop_' + query + '_permission',
        args: {
          table: { name: table, schema: currentSchema },
          role: role,
        },
      };
      const createQuery = {
        type: 'create_' + query + '_permission',
        args: {
          table: { name: table, schema: currentSchema },
          role: role,
          permission: permissionsState[query],
        },
      };
      permissionsUpQueries.push(deleteQuery);
      permissionsDownQueries.push(createQuery);
    }

    if (changeType === permChangeTypes.save) {
      const createQuery = {
        type: 'create_' + query + '_permission',
        args: {
          table: { name: table, schema: currentSchema },
          role: role,
          permission: permissionsState[query],
        },
      };

      if (query === 'insert' && 'localSet' in permissionsState[query]) {
        // Convert insert set array to Object
        if (permissionsState[query].isSetConfigChecked) {
          const newSet = {};
          permissionsState[query].localSet.forEach(s => {
            if (s.key) {
              newSet[s.key] = s.value;
            }
          });
          permissionsState[query].set = { ...newSet };
        } else {
          permissionsState[query].set = {};
        }
        // delete redundant keys
        delete permissionsState[query].isSetConfigChecked;
        delete permissionsState[query].localSet;
      }

      //
      const deleteQuery = {
        type: 'drop_' + query + '_permission',
        args: {
          table: { name: table, schema: currentSchema },
          role: role,
        },
      };

      permissionsUpQueries.push(createQuery);
      permissionsDownQueries.push(deleteQuery);
    }

    // Apply migration
    const migrationName =
      changeType +
      '_permission_' +
      role +
      '_' +
      currentSchema +
      '_table_' +
      table;

    const requestMsg = getIngForm(changeType) + ' Permissions...';
    const successMsg = 'Permissions ' + getEdForm(changeType);
    const errorMsg = getIngForm(changeType) + ' permissions failed';

    const customOnSuccess = () => {
      if (changeType === permChangeTypes.save) {
        dispatch(_permSavePermissions());
      } else if (permChangeTypes.delete) {
        dispatch(_permRemoveAccess());
      }
      // reset new role name
      dispatch(permSetRoleName(''));
      // close edit box
      dispatch(permCloseEdit());
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      permissionsUpQueries,
      permissionsDownQueries,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export {
  permChangeTypes,
  permOpenEdit,
  permSetFilter,
  permSetFilterSameAs,
  permToggleColumn,
  permToggleAllColumns,
  permCloseEdit,
  permSetRoleName,
  permChangePermissions,
  permAllowAll,
  permToggleAllowUpsert,
  permToggleAllowAggregation,
  permToggleModifyLimit,
  permCustomChecked,
  permRemoveRole,
  permSetBulkSelect,
  toggleColumn,
  toggleAllColumns,
  getQueriesWithPermColumns,
  getFilterKey,
  getBasePermissionsState,
  updatePermissionsState,
  deleteFromPermissionsState,
  updateBulkSelect,
  updateBulkSameSelect,
  permRemoveMultipleRoles,
  permSetSameSelect,
  applySamePermissionsBulk,
  setConfigValueType,
};
