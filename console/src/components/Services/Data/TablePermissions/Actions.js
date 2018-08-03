import { defaultPermissionsState, defaultQueryPermissions } from '../DataState';
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
export const PERM_CUSTOM_CHECKED = 'ModifyTable/PERM_CUSTOM_CHECKED';

export const PERM_REMOVE_ACCESS = 'ModifyTable/PERM_REMOVE_ACCESS';
export const PERM_SAVE_PERMISSIONS = 'ModifyTable/PERM_SAVE_PERMISSIONS';
export const PERM_CLOSE_EDIT = 'ModifyTable/PERM_CLOSE_EDIT';
export const PERM_SET_ROLE_NAME = 'ModifyTable/PERM_SET_ROLE_NAME';

const queriesWithPermColumns = ['select', 'update'];
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
const permToggleEnableLimit = checked => ({
  type: PERM_TOGGLE_ENABLE_LIMIT,
  data: checked,
});
const permToggleModifyLimit = limit => ({
  type: PERM_TOGGLE_MODIFY_LIMIT,
  data: limit,
});
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
      _permissions[q] = rolePermissions.permissions[q];
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

const permChangePermissions = changeType => {
  return (dispatch, getState) => {
    const allSchemas = getState().tables.allSchemas;
    const currentSchema = getState().tables.currentSchema;
    const permissionsState = getState().tables.modify.permissionsState;

    const table = permissionsState.table;
    const role = permissionsState.role;
    const query = permissionsState.query;

    const tableSchema = allSchemas.find(t => t.table_name === table);
    const currRolePermissions = tableSchema.permissions.find(
      p => p.role_name === role
    );

    const permissionsUpQueries = [];
    const permissionsDownQueries = [];

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
  permToggleEnableLimit,
  permToggleModifyLimit,
  permCustomChecked,
  toggleColumn,
  toggleAllColumns,
  queriesWithPermColumns,
  getFilterKey,
  getBasePermissionsState,
  updatePermissionsState,
  deleteFromPermissionsState,
};
