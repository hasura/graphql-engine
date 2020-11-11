import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import { getPermissionFilterString } from '../PermissionsSummary/utils';
import { getLegacyOperator, allOperators } from './PermissionBuilder/utils';
import { escapeRegExp } from '../utils';
import { UNSAFE_keys } from '../../../Common/utils/tsUtils';

type FilterType = 'check' | 'filter';
export type BaseQueryType = 'select' | 'update' | 'insert' | 'delete';
export interface FilterState {
  [key: string]: BaseQueryType;
}

type DisplayQueryType =
  | Exclude<BaseQueryType, 'update'>
  | 'post update'
  | 'pre update';

type PermissionsState = {
  query: BaseQueryType;
  filterType: FilterType;
} & {
  [key in BaseQueryType]: string;
};

export const filterTypeDisplayName: Record<FilterType, DisplayQueryType> = {
  filter: 'pre update',
  check: 'post update',
};

export const updateFilterTypeLabel: Record<FilterType, React.ReactElement> = {
  filter: <b>Pre-update check</b>,
  check: (
    <>
      <b>Post-update check</b> <i>(optional)</i>
    </>
  ),
};

const tooltipMsg: Record<FilterType, string> = {
  filter: 'Only rows satisfying the check will be updatable',
  check: 'Rows must satisfy the check after update',
};

export const getUpdateTooltip = (filterType: FilterType) => (
  <Tooltip id={`tooltip-update-${filterType}`}>
    {tooltipMsg[filterType]}
  </Tooltip>
);

const getOptionsForUpdate = (
  currentFilterType: FilterType,
  currentQueryType: BaseQueryType
) => {
  if (currentQueryType !== 'update') {
    return ['check', 'filter'];
  }
  if (currentFilterType === 'check') return ['filter'];
  return [];
};

// return queries grouped by filterString i.e. { filterString: [query] }
export const getFilterQueries = (
  queryTypes: BaseQueryType[],
  { query, ...permissionsState }: PermissionsState,
  filterType: FilterType
) => {
  const filterQueries: Record<string, DisplayQueryType[]> = {};
  queryTypes.forEach(queryType => {
    if (queryType === 'update') {
      const options = getOptionsForUpdate(filterType, query);
      options.forEach(fType => {
        const filterString = getPermissionFilterString(
          permissionsState[queryType],
          queryType,
          false,
          fType
        );
        if (filterString) {
          filterQueries[filterString] = filterQueries[filterString] || [];
          filterQueries[filterString].push(
            filterTypeDisplayName[fType as FilterType]
          );
        }
      });
    } else {
      if (queryType === query) {
        return;
      }

      let queryFilterString = '';
      if (permissionsState[queryType]) {
        queryFilterString = getPermissionFilterString(
          permissionsState[queryType],
          queryType,
          false
        );
      }

      if (queryFilterString) {
        filterQueries[queryFilterString] =
          filterQueries[queryFilterString] || [];
        filterQueries[queryFilterString].push(queryType);
      }
    }
  });

  return filterQueries || {};
};

export const getDefaultFilterType = (query: BaseQueryType) =>
  query === 'insert' ? 'check' : 'filter';

// replace legacy operator values
type FilterString = Partial<{ check: string; filter: string }>;
export const replaceLegacyOperators = (filterString: FilterString) => {
  const newFilterString = { ...filterString };

  allOperators.forEach(operator => {
    const currentString = `"${operator}"`;
    const legacyString = `"${getLegacyOperator(operator)}"`;

    UNSAFE_keys(newFilterString).forEach(key => {
      newFilterString[key] = newFilterString[key]!.replace(
        new RegExp(escapeRegExp(legacyString), 'g'),
        currentString
      );
    });
  });

  return newFilterString;
};

export const getAllowedFilterKeys = (query: BaseQueryType): FilterType[] => {
  switch (query) {
    case 'insert':
      return ['check'];
    case 'update':
      return ['filter', 'check'];
    default:
      return ['filter'];
  }
};

export const getQuerySingleRowMutation = (query: BaseQueryType) => {
  switch (query) {
    case 'insert':
      return 'insert_one';
    case 'update':
      return 'update_by_pk';
    case 'delete':
      return 'delete_by_pk';
    default:
      return '';
  }
};

type PermissionsIcon = 'fullAccess' | 'partialAccess' | 'noAccess';
type RolePermissions = {
  [role: string]: {
    [query in BaseQueryType]: {
      columns: (string | '*')[];
      computed_fields: (string | '*')[];
    } & {
      [key in FilterType]: Record<string, any>;
    };
  };
};
export const getPermissionsIcon = (
  role: 'admin' | string,
  rolePermissions: RolePermissions,
  query: BaseQueryType,
  schemaColumns: any[],
  computedFields: { scalar: any[] }
): PermissionsIcon => {
  if (role === 'admin') {
    return 'fullAccess';
  }

  if (!rolePermissions[role]) {
    return 'noAccess';
  }

  const permissions = rolePermissions[role][query];
  if (!permissions) {
    return 'noAccess';
  }

  const filterKeys = getAllowedFilterKeys(query);
  const checkColumns = query !== 'delete';
  const checkComputedFields = query === 'select';

  if (!filterKeys.every(key => JSON.stringify(permissions[key]) === '{}')) {
    return 'partialAccess';
  }

  if (
    checkColumns &&
    (!permissions.columns ||
      (!permissions.columns.includes('*') &&
        permissions.columns.length !== schemaColumns.length))
  ) {
    return 'partialAccess';
  }

  if (
    checkComputedFields &&
    computedFields.scalar.length &&
    (!permissions.computed_fields ||
      (permissions.computed_fields.includes('*') &&
        permissions.computed_fields.length !== computedFields.scalar.length))
  ) {
    return 'partialAccess';
  }

  return 'fullAccess';
};
