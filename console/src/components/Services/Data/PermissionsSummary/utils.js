import { getDefaultFilterType } from '../TablePermissions/utils';

export const getAllRoles = allTableSchemas => {
  const _allRoles = [];

  allTableSchemas.forEach(tableSchema => {
    if (tableSchema.permissions) {
      tableSchema.permissions.forEach(p => {
        if (!_allRoles.includes(p.role_name)) {
          _allRoles.push(p.role_name);
        }
      });
    }
  });

  _allRoles.sort();

  return _allRoles;
};

export const getTablePermissionsByRoles = tableSchema => {
  const tablePermissionsByRoles = {};

  tableSchema.permissions.forEach(
    p => (tablePermissionsByRoles[p.role_name] = p.permissions)
  );

  return tablePermissionsByRoles;
};

export const getPermissionFilterString = (
  permission,
  query,
  pretty = false,
  filterType
) => {
  const filterKey = filterType || getDefaultFilterType(query);

  let filterString = '';
  if (permission && permission[filterKey]) {
    filterString = pretty
      ? JSON.stringify(permission[filterKey], null, 2)
      : JSON.stringify(permission[filterKey]);
  }

  return filterString;
};

export const getPermissionColumnAccessSummary = (permission, tableFields) => {
  let columnAccessStatus;

  if (!permission) {
    columnAccessStatus = 'no columns';
  } else {
    let noFields = true;
    let allFields = true;

    Object.keys(tableFields).forEach(fieldType => {
      const permissionFields = permission[fieldType] || [];

      noFields = noFields && !permissionFields.length;

      allFields =
        allFields &&
        (permissionFields === '*' ||
          permissionFields.length === tableFields[fieldType].length);
    });

    if (noFields) {
      columnAccessStatus = 'no columns';
    } else if (allFields) {
      columnAccessStatus = 'all columns';
    } else {
      columnAccessStatus = 'partial columns';
    }
  }

  return columnAccessStatus;
};

export const getPermissionRowAccessSummary = filterString => {
  let rowAccessStatus;

  const noAccess = filterString === '';
  const noChecks = filterString === '{}';

  if (noAccess) {
    rowAccessStatus = 'no access';
  } else if (noChecks) {
    rowAccessStatus = 'without any checks';
  } else {
    rowAccessStatus = 'with custom check';
  }

  return rowAccessStatus;
};
