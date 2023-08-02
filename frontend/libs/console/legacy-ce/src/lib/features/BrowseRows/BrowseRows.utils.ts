import { getNullableType, isListType, isObjectType } from 'graphql';
import { TableColumn } from '../DataSource';
import { DataGridOptions } from './components/DataGrid/DataGrid';
import { applyWhereAndSortConditionsToQueryString } from './components/DataGrid/DataGrid.utils';

export const setWhereAndSortToUrl = (options: DataGridOptions) => {
  const searchQueryString = applyWhereAndSortConditionsToQueryString({
    options,
    search: window.location.search,
  });

  if (window.history.pushState) {
    const {
      location: { protocol, host, pathname },
    } = window;

    const newUrl = `${protocol}//${host}${pathname}?${searchQueryString}`;
    window.history.pushState({ path: newUrl }, '', newUrl);
  }
};

export function isScalarGraphQLType(column: TableColumn): boolean {
  if (!column.graphQLProperties?.graphQLType) {
    return false;
  }
  const nullableType = getNullableType(column.graphQLProperties?.graphQLType);
  const isObjectOrArray =
    isListType(nullableType) || isObjectType(nullableType);
  return !isObjectOrArray;
}
