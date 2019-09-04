import React from 'react';
import styles from './PermissionsSummary.scss';

export const permissionsSymbols = {
  fullAccess: (
    <i
      className={'fa fa-check ' + styles.permissionSymbolFA}
      aria-hidden="true"
    />
  ),
  noAccess: (
    <i
      className={'fa fa-times ' + styles.permissionSymbolNA}
      aria-hidden="true"
    />
  ),
  partialAccess: (
    <i
      className={'fa fa-filter ' + styles.permissionSymbolPA}
      aria-hidden="true"
    />
  ),
};

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

const getQueryFilterKey = query => {
  return query === 'insert' ? 'check' : 'filter';
};

export const getPermissionFilterString = (
  permission,
  query,
  pretty = false
) => {
  const filterKey = getQueryFilterKey(query);

  let filterString = '';
  if (permission) {
    filterString = pretty
      ? JSON.stringify(permission[filterKey], null, 2)
      : JSON.stringify(permission[filterKey]);
  }

  return filterString;
};

export const getPermissionColumnAccessSummary = (permission, tableColumns) => {
  let columnAccessStatus;

  if (!permission || !permission.columns.length) {
    columnAccessStatus = 'no columns';
  } else if (
    permission.columns === '*' ||
    permission.columns.length === tableColumns.length
  ) {
    columnAccessStatus = 'all columns';
  } else {
    columnAccessStatus = 'partial columns';
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
