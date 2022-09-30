import React from 'react';
import { persistPageSizeChange } from '@/components/Services/Data/TableBrowseRows/tableUtils';
import { runFilterQuery } from '@/features/BrowseRows';
import type { UserQuery } from '@/features/BrowseRows';
import { useAppDispatch } from '@/store';
import { TableSchema } from '@/components/Services/Data/TableBrowseRows/utils';
import {
  setLimit,
  setOffset,
} from '../../components/Services/Data/TableBrowseRows/FilterActions';
import {
  PaginationWithOnlyNav,
  PaginationWithOnlyNavProps,
} from './PaginationWithOnlyNav';

type PaginationWithOnlyNavContainerProps = {
  limit: PaginationWithOnlyNavProps['limit'];
  offset: PaginationWithOnlyNavProps['offset'];
  onChangePage: PaginationWithOnlyNavProps['changePage'];
  onChangePageSize: PaginationWithOnlyNavProps['changePageSize'];
  pageSize: number;
  rows: PaginationWithOnlyNavProps['rows'];
  tableSchema: TableSchema;
  userQuery: UserQuery;
};

export const PaginationWithOnlyNavContainer = ({
  limit,
  offset,
  onChangePage,
  onChangePageSize,
  pageSize,
  rows,
  tableSchema,
  userQuery,
}: PaginationWithOnlyNavContainerProps) => {
  const dispatch = useAppDispatch();

  const changePageHandler = (newPage: number) => {
    if (offset !== newPage * limit) {
      const newOffset = newPage * limit;
      dispatch(setOffset(newPage * limit));
      dispatch(
        runFilterQuery({
          tableSchema,
          whereAnd: userQuery.where.$and,
          orderBy: userQuery.order_by,
          limit,
          offset: newOffset,
        })
      );
      onChangePage(newPage);
    }
  };

  const changePageSizeHandler = (newPageSize: number) => {
    if (pageSize !== newPageSize) {
      dispatch(setLimit(newPageSize));
      dispatch(setOffset(0));
      dispatch(
        runFilterQuery({
          tableSchema,
          whereAnd: userQuery.where.$and,
          orderBy: userQuery.order_by,
          limit: newPageSize,
          offset: 0,
        })
      );
      persistPageSizeChange(newPageSize);
      onChangePageSize(newPageSize);
    }
  };

  return (
    <PaginationWithOnlyNav
      offset={offset}
      limit={limit}
      changePage={changePageHandler}
      changePageSize={changePageSizeHandler}
      rows={rows}
    />
  );
};
