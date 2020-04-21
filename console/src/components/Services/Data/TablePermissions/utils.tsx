import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import { getPermissionFilterString } from '../PermissionsSummary/utils';
import { getLegacyOperator, allOperators } from './PermissionBuilder/utils';
import { escapeRegExp } from '../utils';
import { UNSAFE_keys } from '../../../Common/utils/tsUtils';

type FilterType = 'check' | 'filter';
type BaseQueryType = 'select' | 'update' | 'insert' | 'delete';

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

export const getAllowedFilterKeys = (query: BaseQueryType) => {
  switch (query) {
    case 'insert':
      return ['check'];
    case 'update':
      return ['filter', 'check'];
    default:
      return ['filter'];
  }
};
