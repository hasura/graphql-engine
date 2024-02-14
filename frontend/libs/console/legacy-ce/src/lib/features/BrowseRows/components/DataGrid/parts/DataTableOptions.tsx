import { Operator, OrderBy, WhereClause } from '../../../../DataSource';
import { Badge } from '../../../../../new-components/Badge';
import { Button } from '../../../../../new-components/Button';
import { DropDown } from '../../../../../new-components/AdvancedDropDown';
import { UseExportRowsReturn } from '../../..';
import clsx from 'clsx';
import React, { useState } from 'react';
import {
  FaChevronLeft,
  FaChevronRight,
  FaFileExport,
  FaFilter,
  FaRegTimesCircle,
  FaSearch,
  FaSortAmountDownAlt,
  FaSortAmountUpAlt,
  FaTimes,
} from 'react-icons/fa';
import type { ExportFileFormat } from '../../..';
import { useFireNotification } from '../../../../../new-components/Notifications';
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
    disableRunQuery?: boolean;
    disableExportSelectedRows: boolean;
    onExportRows: UseExportRowsReturn['onExportRows'];
    onExportSelectedRows: UseExportRowsReturn['onExportRows'];
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
  const twFlexCenter = 'flex items-center';
  return (
    <>
      {whereClauses.map((whereClause, id) => {
        const [columnName, rest] = Object.entries(whereClause)[0];
        const [operator, value] = Object.entries(rest)[0];
        return (
          <Badge color="indigo" key={id}>
            <div className={`gap-3 ${twFlexCenter}`}>
              <span className={`min-h-3 ${twFlexCenter}`}>
                <FaFilter />
              </span>
              <span className={twFlexCenter}>
                {columnName} {operatorMap[operator]} "{value}"
              </span>
              <span className={`min-h-3 ${twFlexCenter}`}>
                <FaRegTimesCircle
                  className="cursor-pointer"
                  onClick={() => removeWhereClause(id)}
                />
              </span>
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
  const twFlexCenter = 'flex items-center';
  return (
    <>
      {orderByClauses.map((orderByClause, id) => (
        <Badge color="yellow" key={id}>
          <div className={`gap-3 ${twFlexCenter}`}>
            <span className={`min-h-3 ${twFlexCenter}`}>
              {orderByClause.type === 'desc' ? (
                <FaSortAmountDownAlt />
              ) : (
                <FaSortAmountUpAlt />
              )}
            </span>
            <span className={twFlexCenter}>
              {orderByClause.column} ({orderByClause.type})
            </span>
            <span className={`min-h-3 ${twFlexCenter}`}>
              <FaRegTimesCircle
                className="cursor-pointer"
                onClick={() => removeOrderByClause(id)}
              />
            </span>
          </div>
        </Badge>
      ))}
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

  const { fireNotification } = useFireNotification();
  const [isExporting, setExporting] = useState(false);
  const onExport = (exportFileFormat: ExportFileFormat) => {
    setExporting(true);
    query
      .onExportRows(exportFileFormat)
      .catch(err =>
        fireNotification({
          title: 'An error occurred',
          message: err?.toString() || err,
          type: 'error',
        })
      )
      .finally(() => {
        setExporting(false);
      });
  };

  const onExportSelectedRows = (exportFileFormat: ExportFileFormat) => {
    setExporting(true);
    query
      .onExportSelectedRows(exportFileFormat)
      .catch(err =>
        fireNotification({
          title: 'An error occurred',
          message: err?.toString() || err,
          type: 'error',
        })
      )
      .finally(() => {
        setExporting(false);
      });
  };

  return (
    <div
      className={clsx(
        'flex items-center px-3.5 py-2 bg-white border border-gray-300',
        query.disableRunQuery ? 'justify-end' : ' justify-between'
      )}
      id="query-options"
    >
      <div className="flex space-x-1.5 items-center">
        <DropDown.Root
          trigger={
            <Button
              type="button"
              mode="default"
              size="sm"
              icon={<FaFileExport />}
              data-testid="@exportBtn"
              title="Export to file"
              isLoading={isExporting}
            >
              Export
            </Button>
          }
          align="start"
          arrow={false}
        >
          <DropDown.SubMenu
            label="Export selected rows"
            disabled={query.disableExportSelectedRows}
          >
            <DropDown.BasicItem
              onClick={() => onExportSelectedRows('CSV')}
              disabled={isExporting || query.disableExportSelectedRows}
            >
              CSV
            </DropDown.BasicItem>
            <DropDown.BasicItem
              onClick={() => onExportSelectedRows('JSON')}
              disabled={isExporting || query.disableExportSelectedRows}
            >
              JSON
            </DropDown.BasicItem>
          </DropDown.SubMenu>
          <DropDown.SubMenu label="Export this table as">
            <DropDown.BasicItem
              onClick={() => onExport('CSV')}
              disabled={isExporting}
            >
              CSV
            </DropDown.BasicItem>
            <DropDown.BasicItem
              onClick={() => onExport('JSON')}
              disabled={isExporting}
            >
              JSON
            </DropDown.BasicItem>
          </DropDown.SubMenu>
        </DropDown.Root>

        <span className="pl-2 pr-2">
          <span className="h-6 border-r-slate-300 border-r border-solid" />
        </span>
        {!query.disableRunQuery && (
          <>
            <Button
              type="button"
              mode="primary"
              size="sm"
              icon={<FaSearch />}
              onClick={query.onQuerySearch}
              data-testid="@runQueryBtn"
              disabled={query.disableRunQuery}
              title="Update filters and sorts on your row data"
            >
              {`Query ${`(${totalQueriesApplied})` || ''}`}
            </Button>
            {totalQueriesApplied > 1 && (
              <Button
                type="button"
                mode="default"
                onClick={query.onRefreshQueryOptions}
                icon={<FaTimes />}
                data-testid="@resetBtn"
                disabled={query.disableRunQuery}
                title="Reset all filters"
                size="sm"
              />
            )}
            {!query.disableRunQuery && (
              <div className="flex flex-wrap gap-3 pl-3 items-center">
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
            )}
          </>
        )}
      </div>

      <div className="flex gap-2 items-center min-w-max">
        <Button
          type="button"
          size="sm"
          icon={<FaChevronLeft />}
          onClick={pagination.goToPreviousPage}
          disabled={pagination.isPreviousPageDisabled}
          data-testid="@prevPageBtn"
          title="Previous page"
        />

        <select
          value={pagination.pageSize}
          onChange={e => {
            pagination.setPageSize(Number(e.target.value));
          }}
          data-testid="@rowSizeSelectInput"
          className="block w-full max-w-xl h-8 min-h-full shadow-sm rounded pl-3 pr-6 py-0.5 border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400"
        >
          {DEFAULT_PAGE_SIZES.map(pageSize => (
            <option key={pageSize} value={pageSize}>
              Show {pageSize} rows
            </option>
          ))}
        </select>
        <Button
          type="button"
          size="sm"
          icon={<FaChevronRight />}
          onClick={pagination.goToNextPage}
          disabled={pagination.isNextPageDisabled}
          data-testid="@nextPageBtn"
          title="Next page"
        />
      </div>
    </div>
  );
};
