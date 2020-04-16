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

export const filterTypeToDisplayName = (filterType: FilterType) => {
  switch (filterType) {
    case 'check':
      return 'pre update';
    case 'filter':
      return 'post update';
    default:
      throw new Error('invalid filter type');
  }
};

export const updateFilterTypeToLabel = (filterType: FilterType) => {
  switch (filterType) {
    case 'check':
      return 'Post-condition check (rows must satisfy the check after update)';
    case 'filter':
      return 'Pre-condition check (only rows satisfying the check will be updatable)';
    default:
      return '';
  }
};

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
            filterTypeToDisplayName(fType as FilterType)
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
