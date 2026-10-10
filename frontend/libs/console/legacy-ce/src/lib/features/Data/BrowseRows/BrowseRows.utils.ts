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
