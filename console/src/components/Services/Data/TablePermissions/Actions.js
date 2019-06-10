import {
  defaultPermissionsState,
  defaultQueryPermissions,
  defaultPresetsState,
} from '../DataState';
import { getEdForm, getIngForm } from '../utils';
import { makeMigrationCall } from '../DataActions';
import dataHeaders from '../Common/Headers';
import { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';
import Endpoints from '../../../../Endpoints';

export const PERM_ADD_TABLE_SCHEMAS = 'ModifyTable/PERM_ADD_TABLE_SCHEMAS';
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
export const PERM_RESET_APPLY_SAME = 'ModifyTable/PERM_RESET_APPLY_SAME';
export const PERM_SET_APPLY_SAME_PERM = 'ModifyTable/PERM_SET_APPLY_SAME_PERM';
export const PERM_DEL_APPLY_SAME_PERM = 'ModifyTable/PERM_DEL_APPLY_SAME_PERM';

export const X_HASURA_CONST = 'x-hasura-';

/* preset operations */
export const SET_PRESET_VALUE = 'ModifyTable/SET_PRESET_VALUE';

export const CREATE_NEW_PRESET = 'ModifyTable/CREATE_NEW_PRESET';

export const DELETE_PRESET = 'ModifyTable/DELETE_PRESET';

/* */

const permChangeTypes = {
  save: 'update',
  delete: 'delete',
};

const permOpenEdit = (tableSchema, role, query) => ({
  type: PERM_OPEN_EDIT,
  tableSchema,
  role,
  query,
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
const permSetApplySamePerm = (index, key, value) => {
  const data = { index, key, value };

  return dispatch => {
    dispatch({ type: PERM_SET_APPLY_SAME_PERM, data: data });
  };
};
const permDelApplySamePerm = index => {
  return dispatch => {
    dispatch({ type: PERM_DEL_APPLY_SAME_PERM, data: index });
  };
};
const permCustomChecked = () => ({ type: PERM_CUSTOM_CHECKED });

const getFilterKey = query => {
  return query === 'insert' ? 'check' : 'filter';
};

const getBasePermissionsState = (tableSchema, role, query) => {
  const _permissions = JSON.parse(JSON.stringify(defaultPermissionsState));

  _permissions.table = tableSchema.table_name;
  _permissions.role = role;
  _permissions.query = query;

  const rolePermissions = tableSchema.permissions.find(
    p => p.role_name === role
  );
  if (rolePermissions) {
    Object.keys(rolePermissions.permissions).forEach(q => {
      const localPresets = [];
      _permissions[q] = rolePermissions.permissions[q];
      // If the query is insert, transform set object if exists to an array
      if (q === 'insert' || q === 'update') {
        // If set is an object
        if (!_permissions[q].columns) {
          _permissions[q].columns = tableSchema.columns.map(c => c.column_name);
        }
        if ('set' in _permissions[q]) {
          if (
            Object.keys(_permissions[q].set).length > 0 &&
            !(_permissions[q].set.length > 0)
          ) {
            Object.keys(_permissions[q].set).map(s => {
              localPresets.push({
                key: s,
                value: _permissions[q].set[s],
              });
            });
          }

          localPresets.push(defaultPresetsState[q]);

          _permissions[q].localPresets = [...localPresets];
        } else {
          // Just to support version changes
          // If user goes from current to previous version and back
          _permissions[q].localPresets = [defaultPresetsState[q]];
          _permissions[q].set = {};
        }
      }
    });
  } else {
    _permissions.newRole = role;
  }

  return _permissions;
};

const permAddTableSchemas = schemaNames => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'select',
        args: {
          table: {
            name: 'hdb_table',
            schema: 'hdb_catalog',
          },
          columns: [
            '*.*',
            {
              name: 'columns',
              columns: ['*.*'],
              order_by: [{ column: 'column_name', type: 'asc', nulls: 'last' }],
            },
          ],
          where: { table_schema: { $in: schemaNames } },
          order_by: [{ column: 'table_name', type: 'asc', nulls: 'last' }],
        },
      }),
    };

    return dispatch(requestAction(url, options)).then(
      data => {
        dispatch({ type: PERM_ADD_TABLE_SCHEMAS, schemas: data });
      },
      error => {
        console.error('Failed to load table schemas: ' + JSON.stringify(error));
      }
    );
  };
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

const updateApplySamePerms = (permissionsState, data, isDelete) => {
  const applySamePerms = [...permissionsState.applySamePermissions];

  if (isDelete) {
    applySamePerms.splice(data, 1);
  } else {
    if (data.index === applySamePerms.length) {
      applySamePerms.push({ table: '', role: '', action: '' });
    }
  }

  applySamePerms[data.index] = { ...applySamePerms[data.index] };
  applySamePerms[data.index][data.key] = data.value;

  return applySamePerms;
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

    const requestMsg = 'Removing permissions...';
    const successMsg = 'Permissions removed';
    const errorMsg = 'Removing permissions failed';

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
    const allSchemas = getState().tables.allSchemas;
    const currentSchema = getState().tables.currentSchema;
    const permissionsState = getState().tables.modify.permissionsState;

    const table = tableSchema.table_name;
    const currentQueryType = permissionsState.query;
    const toBeAppliedPermission = permissionsState[currentQueryType];

    const mainApplyTo = {
      table: table,
      role: permissionsState.role,
      action: currentQueryType,
    };

    const permApplyToList = permissionsState.applySamePermissions.concat([
      mainApplyTo,
    ]);

    const permissionsUpQueries = [];
    const permissionsDownQueries = [];

    let currentPermissions = [];
    allSchemas.forEach(tSchema => {
      currentPermissions = currentPermissions.concat(tSchema.permissions);
    });

    permApplyToList.map(applyTo => {
      const currTableSchema = allSchemas.find(
        tSchema =>
          tSchema.table_name === applyTo.table &&
          tSchema.table_schema === currentSchema
      );
      const currentPermPermission = currTableSchema.permissions.find(el => {
        return el.role_name === applyTo.role;
      });

      if (
        currentPermPermission &&
        currentPermPermission.permissions[applyTo.action]
      ) {
        // existing permission is there. so drop and recreate for down migrations
        const deleteQuery = {
          type: 'drop_' + applyTo.action + '_permission',
          args: {
            table: { name: applyTo.table, schema: currentSchema },
            role: applyTo.role,
          },
        };
        const createQuery = {
          type: 'create_' + applyTo.action + '_permission',
          args: {
            table: { name: applyTo.table, schema: currentSchema },
            role: applyTo.role,
            permission: currentPermPermission.permissions[applyTo.action],
          },
        };
        permissionsUpQueries.push(deleteQuery);
        permissionsDownQueries.push(createQuery);
      }

      // modify query depending on table and action
      const sanitizedPermission = { ...toBeAppliedPermission };
      if (applyTo.table !== table) {
        sanitizedPermission.columns = [];
        sanitizedPermission.set = {};
      }
      if (applyTo.action === 'insert' && currentQueryType !== 'insert') {
        sanitizedPermission.check = sanitizedPermission.filter;
      } else if (applyTo.action !== 'insert' && currentQueryType === 'insert') {
        sanitizedPermission.filter = sanitizedPermission.check;
      }

      // now add normal create and drop permissions
      const createQuery = {
        type: 'create_' + applyTo.action + '_permission',
        args: {
          table: { name: applyTo.table, schema: currentSchema },
          role: applyTo.role,
          permission: sanitizedPermission,
        },
      };
      const deleteQuery = {
        type: 'drop_' + applyTo.action + '_permission',
        args: {
          table: { name: applyTo.table, schema: currentSchema },
          role: applyTo.role,
        },
      };
      permissionsUpQueries.push(createQuery);
      permissionsDownQueries.push(deleteQuery);
    });

    // Apply migration
    const migrationName =
      'apply_same_permissions_' + currentSchema + '_table_' + table;

    const requestMsg = 'Applying Permissions';
    const successMsg = 'Permission Changes Applied';
    const errorMsg = 'Permission Changes Failed';

    const customOnSuccess = () => {
      // reset new role name
      dispatch(permSetRoleName(''));
      // close edit box
      dispatch(permCloseEdit());
      // reset checkbox selections
      dispatch({ type: PERM_RESET_APPLY_SAME });
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
    const prevPermissionsState = {
      ...getState().tables.modify.prevPermissionState,
    };
    const limitEnabled = permissionsState.limitEnabled;

    const table = permissionsState.table;
    const role = permissionsState.role;
    const query = permissionsState.query;

    const tableSchema = allSchemas.find(
      t => t.table_name === table && t.table_schema === currentSchema
    );
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
          permission: prevPermissionsState[query],
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

      if (
        (query === 'insert' || query === 'update') &&
        'localPresets' in permissionsState[query]
      ) {
        // Convert preset array to Object
        const presetsObject = {};
        permissionsState[query].localPresets.forEach(s => {
          if (s.key) {
            presetsObject[s.key] = s.value;
          }
        });
        permissionsState[query].set = { ...presetsObject };
      }

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

    // Reverse order of down migration
    permissionsDownQueries.reverse();

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
  permAddTableSchemas,
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
  getFilterKey,
  getBasePermissionsState,
  updatePermissionsState,
  deleteFromPermissionsState,
  updateBulkSelect,
  updateApplySamePerms,
  permRemoveMultipleRoles,
  permSetApplySamePerm,
  permDelApplySamePerm,
  applySamePermissionsBulk,
};
