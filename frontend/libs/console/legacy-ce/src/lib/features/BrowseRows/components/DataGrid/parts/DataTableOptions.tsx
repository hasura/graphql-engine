import { Operator, OrderBy, WhereClause } from '@/features/DataSource';
import { Badge } from '@/new-components/Badge';
import { Button } from '@/new-components/Button';
import React from 'react';
import {
  FaChevronLeft,
  FaChevronRight,
  FaSearch,
  FaUndo,
  FaRegTimesCircle,
  FaSortAmountUpAlt,
  FaFilter,
} from 'react-icons/fa';
import { DEFAULT_PAGE_SIZES } from '../constants';

interface DataTableOptionsProps {
  query: {
    onQuerySearch: () => void;
    onRefreshQueryOptions: () => void;
    orderByClauses: OrderBy[];
    whereClauses: WhereClause[];
    supportedOperators: Operator[];
    removeWhereClause: (id: number) => void;
    removeOrderByClause: (id: number) => void;
  };
  pagination: {
    goToPreviousPage: () => void;
    goToNextPage: () => void;
    isPreviousPageDisabled: boolean;
    isNextPageDisabled: boolean;
    pageSize: number;
    setPageSize: (pageSize: number) => void;
  };
}

const DisplayWhereClauses = ({
  whereClauses,
  operatorMap,
  removeWhereClause,
}: {
  whereClauses: WhereClause[];
  operatorMap: Record<string, string>;
  removeWhereClause: (id: number) => void;
}) => {
  return (
    <>
      {whereClauses.map((whereClause, id) => {
        const [columnName, rest] = Object.entries(whereClause)[0];
        const [operator, value] = Object.entries(rest)[0];
        return (
          <Badge color="indigo" className="mb-3">
            <div className="flex gap-3 items-center">
              <FaFilter />
              <span>
                {columnName} {operatorMap[operator]} {value}
              </span>
              <FaRegTimesCircle
                onClick={() => {
                  removeWhereClause(id);
                }}
              />
            </div>
          </Badge>
        );
      })}
    </>
  );
};

const DisplayOrderByClauses = ({
  orderByClauses,
  removeOrderByClause,
}: {
  orderByClauses: OrderBy[];
  removeOrderByClause: (id: number) => void;
}) => {
  return (
    <>
      {orderByClauses.map((orderByClause, id) => {
        return (
          <Badge color="yellow" className="mb-3">
            <div className="flex gap-3 items-center">
              <FaSortAmountUpAlt />
              <span>
                {orderByClause.column} ({orderByClause.type})
              </span>
              <FaRegTimesCircle
                onClick={() => {
                  removeOrderByClause(id);
                }}
              />
            </div>
          </Badge>
        );
      })}
    </>
  );
};

export const DataTableOptions = (props: DataTableOptionsProps) => {
  const { query, pagination } = props;
  const operatorMap = query.supportedOperators.reduce<Record<string, string>>(
    (acc, operator) => {
      return { ...acc, [operator.value]: operator.name };
    },
    {}
  );

  const totalQueriesApplied =
    query.whereClauses.length + query.orderByClauses.length;
  return (
    <div
      className="flex items-center justify-between p-4 bg-white border"
      id="query-options"
    >
      <div className="flex space-x-1.5">
        <Button
          type="button"
          mode="primary"
          icon={<FaSearch />}
          onClick={query.onQuerySearch}
          data-testid="@runQueryBtn"
        >
          Query{' '}
          <span>{totalQueriesApplied ? `(${totalQueriesApplied})` : ''}</span>
        </Button>
        <Button
          type="button"
          mode="default"
          onClick={query.onRefreshQueryOptions}
          icon={<FaUndo />}
          data-testid="@resetBtn"
        />
        <div className="flex-wrap pl-3">
          <DisplayWhereClauses
            operatorMap={operatorMap}
            whereClauses={query.whereClauses}
            removeWhereClause={query.removeWhereClause}
          />
          <DisplayOrderByClauses
            orderByClauses={query.orderByClauses}
            removeOrderByClause={query.removeOrderByClause}
          />
        </div>
      </div>

      <div className="flex gap-2 items-center min-w-max">
        <Button
          type="button"
          icon={<FaChevronLeft />}
          onClick={pagination.goToPreviousPage}
          disabled={pagination.isPreviousPageDisabled}
          data-testid="@prevPageBtn"
        />

        <select
          value={pagination.pageSize}
          onChange={(e) => {
            pagination.setPageSize(Number(e.target.value));
          }}
          data-testid="@rowSizeSelectInput"
          className="block w-full max-w-xl h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400"
        >
          {DEFAULT_PAGE_SIZES.map((pageSize) => (
            <option key={pageSize} value={pageSize}>
              Show {pageSize} rows
            </option>
          ))}
        </select>
        <Button
          type="button"
          icon={<FaChevronRight />}
          onClick={pagination.goToNextPage}
          disabled={pagination.isNextPageDisabled}
          data-testid="@nextPageBtn"
        />
      </div>
    </div>
  );
};
