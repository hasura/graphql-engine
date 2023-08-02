import React, { useState } from 'react';
import { DEFAULT_PAGE_NUMBER, DEFAULT_PAGE_SIZE } from '../constants';
import { useCheckRows } from '.';
import { paginate, search } from '../utils';

export function usePaginatedSearchableList<TData extends { id: string }>({
  data,
  filterFn,
  defaultQuery,
}: {
  data: TData[];
  filterFn: (searchText: string, item: TData) => boolean;
  defaultQuery?: string;
}) {
  const [pageNumber, setPageNumber] = useState(DEFAULT_PAGE_NUMBER);
  const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
  const [searchText, setSearchText] = useState(defaultQuery ?? '');

  const searchIsActive = !!searchText.length;

  const filteredData = React.useMemo(
    () => search<TData>({ data, searchText, filterFn }),
    [data, filterFn, searchText]
  );

  const { data: paginatedData, totalPages } = React.useMemo(
    () => paginate<TData>({ data: filteredData, pageNumber, pageSize }),
    [filteredData, pageNumber, pageSize]
  );

  const rowsToBeChecked = searchIsActive ? filteredData : paginatedData;

  const checkData = useCheckRows(rowsToBeChecked, filteredData, data);

  const getCheckedItems = React.useCallback(
    () => data.filter(d => checkData.checkedIds.includes(d.id)),
    [checkData.checkedIds, data]
  );

  const handleSearch = React.useCallback((searchQuery: string) => {
    setPageNumber(DEFAULT_PAGE_NUMBER);
    setSearchText(searchQuery);
  }, []);

  const incrementPage = React.useCallback(() => {
    setPageNumber(currentPage =>
      currentPage >= data.length / pageSize ? currentPage : currentPage + 1
    );
  }, [data.length, pageSize]);

  const decrementPage = React.useCallback(() => {
    setPageNumber(currentPage =>
      currentPage === 1 ? currentPage : currentPage - 1
    );
  }, []);

  const goToFirstPage = React.useCallback(() => {
    setPageNumber(() => 1);
  }, []);

  const goToLastPage = React.useCallback(() => {
    setPageNumber(() => totalPages);
  }, [totalPages]);

  return {
    pageNumber,
    setPageNumber,
    pageSize,
    setPageSize,
    incrementPage,
    decrementPage,
    goToFirstPage,
    goToLastPage,
    totalPages,
    searchIsActive,
    handleSearch,
    checkData,
    filteredData,
    paginatedData,
    getCheckedItems,
    dataSize: filteredData.length,
  };
}

export type PaginatedSearchableListProps = ReturnType<
  typeof usePaginatedSearchableList
>;
