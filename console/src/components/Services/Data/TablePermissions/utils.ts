import { getPermissionFilterString } from '../PermissionsSummary/utils';

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

const getOptionsForUpdate = (
  currentFilterType: FilterType,
  currentQueryType: BaseQueryType
) => {
  const options = ['check', 'filter'];
  if (currentQueryType !== 'update') {
    return options;
  }
  return options.filter(o => o !== currentFilterType);
};

// return queries grouped by filterString i.e. { filterString: [query] }
export const getFilterQueries = (
  queryTypes: BaseQueryType[],
  { query, filterType, ...permissionsState }: PermissionsState
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
