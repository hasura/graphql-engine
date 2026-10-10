import { OrderBy } from '@hasura/shared/types';
import React from 'react';

// Types for sorts
export type SetSorts = (s: OrderBy[]) => void;

/*
 * Local state for the pagination component
 */
export type PaginationState = {
  sorts: OrderBy[];
  limit: number;
  offset: number;
};

const defaultSort: OrderBy = {
  column: 'created_at',
  type: 'asc',
  nulls: 'last',
};

export const usePagination = (initialState?: Partial<PaginationState>) => {
  const [paginationState, setPaginationState] = React.useState<PaginationState>(
    {
      limit: 10,
      offset: 0,
      sorts: [defaultSort],
      ...initialState,
    },
  );

  return {
    paginationState,
    setPaginationState,
  };
};

export type UsePaginationState = ReturnType<typeof usePagination>;
