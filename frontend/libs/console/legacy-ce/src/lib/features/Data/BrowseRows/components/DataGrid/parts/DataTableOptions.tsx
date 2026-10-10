import {
  Badge,
  Button,
  DropdownMenu,
  hasuraToast,
  Pagination,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { useState } from 'react';
import {
  FaFileExport,
  FaFilter,
  FaTimesCircle,
  FaSearch,
  FaSortAmountDownAlt,
  FaSortAmountUpAlt,
  FaTimes,
} from 'react-icons/fa';
import { OrderBy, WhereClause } from '@hasura/shared/types';
import {
  ExportFileFormat,
  Operator,
  TableRow,
} from '@hasura/metadata/data-source';

interface DataTableOptionsProps {
  query: {
    onQuerySearch: () => void;
    onDeleteRows: () => void;
    onRefreshQueryOptions: () => void;
    orderByClauses: OrderBy[];
    whereClauses: WhereClause[];
    supportedOperators: Operator[];
    removeWhereClause: (id: number) => void;
    removeOrderByClause: (id: number) => void;
    disableRunQuery?: boolean;
    selectedRowsCount: number;
    isDeleting: boolean;
    onExportRows: (
      exportFileFormat: ExportFileFormat,
    ) => Promise<TableRow[] | Error>;
    onExportSelectedRows: (
      exportFileFormat: ExportFileFormat,
    ) => Promise<TableRow[] | Error>;
  };
  pagination: {
    goToPreviousPage: () => void;
    goToNextPage: () => void;
    isPreviousPageDisabled: boolean;
    isNextPageDisabled: boolean;
    pageSize: number;
    pageIndex: number;
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
                {columnName} {operatorMap[operator]} &quot;{value}&quot;
              </span>
              <span className={`min-h-3 ${twFlexCenter}`}>
                <FaTimesCircle
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
              <FaTimesCircle
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
    {},
  );

  const totalQueriesApplied =
    query.whereClauses.length + query.orderByClauses.length;

  const [isExporting, setExporting] = useState(false);
  const onExport = (exportFileFormat: ExportFileFormat) => {
    setExporting(true);
    query
      .onExportRows(exportFileFormat)
      .catch((err) =>
        hasuraToast({
          title: 'An error occurred',
          message: err?.toString() || err,
          type: 'error',
        }),
      )
      .finally(() => {
        setExporting(false);
      });
  };

  const onExportSelectedRows = (exportFileFormat: ExportFileFormat) => {
    setExporting(true);
    query
      .onExportSelectedRows(exportFileFormat)
      .catch((err) =>
        hasuraToast({
          title: 'An error occurred',
          message: err?.toString() || err,
          type: 'error',
        }),
      )
      .finally(() => {
        setExporting(false);
      });
  };

  const disabled = query.isDeleting;

  return (
    <Flex
      align="center"
      justify="between"
      className={'px-3.5 py-2'}
      id="query-options"
    >
      <Flex align="center" gap="2">
        {query.selectedRowsCount > 0 && (
          <Button
            mode="destructive"
            onClick={query.onDeleteRows}
            loading={query.isDeleting}
          >
            Delete {query.selectedRowsCount} rows
          </Button>
        )}
        <DropdownMenu.Root
          items={[
            <DropdownMenu.Label key="export-selected-label">
              Export selected rows
            </DropdownMenu.Label>,
            <DropdownMenu.Item
              key="export-selected-csv"
              onClick={() => onExportSelectedRows('CSV')}
              disabled={isExporting || !query.selectedRowsCount}
            >
              CSV
            </DropdownMenu.Item>,
            <DropdownMenu.Item
              key="export-selected-json"
              onClick={() => onExportSelectedRows('JSON')}
              disabled={isExporting || !query.selectedRowsCount}
            >
              JSON
            </DropdownMenu.Item>,
            <DropdownMenu.Separator key="export-separator" />,
            <DropdownMenu.Label key="export-table-label">
              Export this table as
            </DropdownMenu.Label>,
            <DropdownMenu.Item
              key="export-table-csv"
              onClick={() => onExport('CSV')}
              disabled={isExporting}
            >
              CSV
            </DropdownMenu.Item>,
            <DropdownMenu.Item
              key="export-table-json"
              onClick={() => onExport('JSON')}
              disabled={isExporting}
            >
              JSON
            </DropdownMenu.Item>,
          ]}
        >
          <Button
            type="button"
            mode="default"
            leftIcon={FaFileExport}
            data-testid="@exportBtn"
            title="Export to file"
            loading={isExporting}
            disabled={disabled}
          >
            Export
          </Button>
        </DropdownMenu.Root>

        <span className="h-12 border-r-slate-300 border-r border-solid" />
        {!query.disableRunQuery && (
          <>
            <Button
              type="button"
              mode="primary"
              leftIcon={FaSearch}
              onClick={query.onQuerySearch}
              data-testid="@runQueryBtn"
              disabled={query.disableRunQuery || disabled}
              title="Update filters and sorts on your row data"
            >
              {`Query ${totalQueriesApplied ? `(${totalQueriesApplied})` : ''}`}
            </Button>
            {totalQueriesApplied > 1 && (
              <Button
                type="button"
                mode="default"
                onClick={query.onRefreshQueryOptions}
                data-testid="@resetBtn"
                disabled={query.disableRunQuery || disabled}
                title="Reset all filters"
                leftIcon={FaTimes}
              >
                Clear All
              </Button>
            )}
            {!query.disableRunQuery && (
              <Flex wrap="wrap" gap="3" align="center" className="pl-3">
                <DisplayWhereClauses
                  operatorMap={operatorMap}
                  whereClauses={query.whereClauses}
                  removeWhereClause={query.removeWhereClause}
                />
                <DisplayOrderByClauses
                  orderByClauses={query.orderByClauses}
                  removeOrderByClause={query.removeOrderByClause}
                />
              </Flex>
            )}
          </>
        )}
      </Flex>

      <Pagination
        pageIndex={pagination.pageIndex}
        pageSize={pagination.pageSize}
        setPageSize={pagination.setPageSize}
        canNext={!pagination.isNextPageDisabled}
        goToNextPage={pagination.goToNextPage}
        goToPreviousPage={pagination.goToPreviousPage}
      />
    </Flex>
  );
};
