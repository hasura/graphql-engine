import {
  defaultPermissionsState,
  defaultQueryPermissions,
} from '../DataState';
import { getEdForm, getIngForm } from '../utils';
import { makeMigrationCall, fetchRoleList } from '../DataActions';
import {
  findTable,
  generateTableDef,
  getSchemaTables,
  getTableDef,
  getTablePermissions,
} from '../../../Common/utils/pgUtils';
import {
  getCreatePermissionQuery,
  getDropPermissionQuery,
} from '../../../Common/utils/v1QueryUtils';

export const PERM_OPEN_EDIT = 'ModifyTable/PERM_OPEN_EDIT';
export const PERM_SET_FILTER = 'ModifyTable/PERM_SET_FILTER';
export const PERM_SET_FILTER_SAME_AS = 'ModifyTable/PERM_SET_FILTER_SAME_AS';
export const PERM_TOGGLE_FIELD = 'ModifyTable/PERM_TOGGLE_FIELD';
export const PERM_TOGGLE_ALL_FIELDS = 'ModifyTable/PERM_TOGGLE_ALL_FIELDS';
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
const permToggleField = (fieldType, fieldName) => ({
  type: PERM_TOGGLE_FIELD,
  fieldType,
  fieldName,
});
const permToggleAllFields = allFields => ({
  type: PERM_TOGGLE_ALL_FIELDS,
  allFields,
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

const getBasePermissionsState = (tableSchema, role, query, isNewRole) => {
  const _permissions = JSON.parse(JSON.stringify(defaultPermissionsState));

  _permissions.table = tableSchema.table_name;
  _permissions.role = role;
  _permissions.query = query;

  const rolePermissions = tableSchema.permissions.find(
    p => p.role_name === role
  );

  if (rolePermissions) {
    Object.keys(rolePermissions.permissions).forEach(q => {
      _permissions[q] = rolePermissions.permissions[q];

      if (q === 'insert' || q === 'update') {
        if (!_permissions[q].columns) {
          _permissions[q].columns = [];
        }

        if (!_permissions[q].set) {
          _permissions[q].set = {};
        }
      }
    });
  }

  if (isNewRole) {
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

const updateApplySamePerms = (permissionsState, data, isDelete) => {
  const applySamePerms = [...permissionsState.applySamePermissions];

  if (isDelete) {
    applySamePerms.splice(data, 1);
  } else {
    if (data.index === applySamePerms.length) {
      applySamePerms.push({
        table: permissionsState.table,
        action: permissionsState.query,
        role: '',
      });
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

// fieldType: columns / computed_fields
const toggleAllFields = (permissions, allFields, fieldType) => {
  let allFieldsSelected = true;

  Object.keys(allFields).forEach(fType => {
    const currSelected = permissions ? permissions[fType] : [];

    const allSelected = currSelected.length === allFields[fType].length;

    allFieldsSelected = allFieldsSelected && allSelected;
  });

  return allFieldsSelected ? [] : allFields[fieldType];
};

// fieldType: columns / computed_fields
const toggleField = (permissions, fieldName, fieldType) => {
  const currFields =
    permissions && permissions[fieldType] ? permissions[fieldType] : [];

  let _newFields = currFields;

  const fieldIndex = currFields.indexOf(fieldName);
  if (fieldIndex === -1) {
    _newFields.push(fieldName);
  } else {
    _newFields.splice(fieldIndex, 1);
  }

  _newFields = _newFields.sort();

  return _newFields;
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
      // fetch all roles
      dispatch(fetchRoleList());
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
      // fetch all roles
      dispatch(fetchRoleList());
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

const applySamePermissionsBulk = (tableSchema, arePermissionsModified) => {
  return (dispatch, getState) => {
    const permissionsUpQueries = [];
    const permissionsDownQueries = [];

    const allSchemas = getState().tables.allSchemas;
    const currentSchema = getState().tables.currentSchema;
    const permissionsState = getState().tables.modify.permissionsState;

    const table = tableSchema.table_name;
    const currentQueryType = permissionsState.query;
    const toBeAppliedPermission = permissionsState[currentQueryType];

    const permApplyToList = permissionsState.applySamePermissions.filter(
      applyTo => applyTo.table && applyTo.action && applyTo.role
    );

    if (arePermissionsModified) {
      const mainApplyTo = {
        table: table,
        action: currentQueryType,
        role: permissionsState.role,
      };

      permApplyToList.push(mainApplyTo);
    }

    permApplyToList.map(applyTo => {
      const currTableSchema = findTable(
        allSchemas,
        generateTableDef(applyTo.table, currentSchema)
      );

      const currentPermPermission = currTableSchema.permissions.find(
        el => el.role_name === applyTo.role
      );

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
        permissionsDownQueries.unshift(createQuery);
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
      permissionsDownQueries.unshift(deleteQuery);
    });

    // Apply migration
    const migrationName =
      'apply_same_permissions_' + currentSchema + '_table_' + table;

    const requestMsg = 'Applying permissions';
    const successMsg = 'Permission changes applied';
    const errorMsg = 'Permission changes failed';

    const customOnSuccess = () => {
      // reset new role name
      dispatch(permSetRoleName(''));
      // close edit box
      dispatch(permCloseEdit());
      // reset checkbox selections
      dispatch({ type: PERM_RESET_APPLY_SAME });
      // fetch all roles
      dispatch(fetchRoleList());
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

const copyRolePermissions = (
  fromRole,
  tableNameWithSchema,
  action,
  toRoles,
  onSuccess
) => {
  return (dispatch, getState) => {
    const permissionsUpQueries = [];
    const permissionsDownQueries = [];

    const allSchemas = getState().tables.allSchemas;
    const currentSchema = getState().tables.currentSchema;

    let tables;
    if (tableNameWithSchema === 'all') {
      tables = getSchemaTables(allSchemas, currentSchema);
    } else {
      const fromTableDef = generateTableDef(null, null, tableNameWithSchema);
      tables = [findTable(allSchemas, fromTableDef)];
    }

    tables.forEach(table => {
      const tableDef = getTableDef(table);

      let actions;
      if (action === 'all') {
        actions = ['select', 'insert', 'update', 'delete'];
      } else {
        actions = [action];
      }

      toRoles.forEach(toRole => {
        actions.forEach(_action => {
          const currPermissions = getTablePermissions(table, toRole, _action);
          const toBeAppliedPermissions = getTablePermissions(
            table,
            fromRole,
            _action
          );

          if (currPermissions) {
            // existing permission is there. so drop and recreate for down migrations
            const deleteQuery = getDropPermissionQuery(
              _action,
              tableDef,
              toRole
            );
            const createQuery = getCreatePermissionQuery(
              _action,
              tableDef,
              toRole,
              currPermissions
            );

            permissionsUpQueries.push(deleteQuery);
            permissionsDownQueries.push(createQuery);
          }

          if (toBeAppliedPermissions) {
            // now add normal create and drop permissions
            const createQuery = getCreatePermissionQuery(
              _action,
              tableDef,
              toRole,
              toBeAppliedPermissions
            );
            const deleteQuery = getDropPermissionQuery(
              _action,
              tableDef,
              toRole
            );

            permissionsUpQueries.push(createQuery);
            permissionsDownQueries.push(deleteQuery);
          }
        });
      });
    });

    // Apply migration
    const migrationName =
      'copy_role_' +
      fromRole +
      '_' +
      action +
      '_query_permissions_for_' +
      tableNameWithSchema.replace('.', '_') +
      '_table_to_' +
      toRoles.join('_');

    const requestMsg = 'Copying permissions';
    const successMsg = 'Permissions copied';
    const errorMsg = 'Permissions copy failed';

    const customOnSuccess = () => {
      onSuccess();
      // fetch all roles
      dispatch(fetchRoleList());
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
      // fetch all roles
      dispatch(fetchRoleList());
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
  permToggleField,
  permToggleAllFields,
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
  toggleField,
  toggleAllFields,
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
  copyRolePermissions,
};
