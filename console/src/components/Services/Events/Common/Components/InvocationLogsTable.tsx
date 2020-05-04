import React from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';
import {
  FilterState,
  SetFilterState,
  RunQuery,
} from '../../../../Common/FilterQuery/utils';
// import { convertDateTimeToLocale } from '../../../../Common/utils/jsUtils';
import { ordinalColSort } from '../../../Data/utils';
import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';

type Props = {
  rows: any[];
  count?: number;
  filterState: FilterState;
  setFilterState: SetFilterState;
  runQuery: RunQuery;
  columns: string[];
  triggerName: string;
};

const InvocationLogsTable = (props: Props) => {
  const { rows, filterState, setFilterState, runQuery, columns, count } = props;

  if (rows.length === 0) {
    return <div className={styles.add_mar_top}>No data available</div>;
  }

  const sortedColumns = columns.sort(ordinalColSort);

  let shouldSortColumn = true;

  const sortByColumn = (col: string) => {
    // Remove all the existing order_bys

    const existingColSort = filterState.sorts.find(s => s.column === col);
    if (existingColSort && existingColSort.type === 'asc') {
      setFilterState.sorts([
        {
          column: col,
          type: 'desc',
        },
      ]);
    } else {
      setFilterState.sorts([
        {
          column: col,
          type: 'asc',
        },
      ]);
    }

    runQuery();
  };

  const changePage = (page: number) => {
    if (filterState.offset !== page * filterState.limit) {
      runQuery(page * filterState.limit);
    }
  };

  const changePageSize = (size: number) => {
    if (filterState.limit !== size) {
      runQuery(undefined, size);
    }
  };

  const gridHeadings = sortedColumns.map(column => {
    return {
      Header: column,
      accessor: column,
    };
  });

  return (
    <ReactTable
      data={rows}
      columns={gridHeadings}
      minRows={0}
      resizable
      manual
      getTheadThProps={(finalState, some, column) => ({
        onClick: () => {
          if (
            column &&
            column.Header &&
            shouldSortColumn &&
            column.Header !== 'Actions'
          ) {
            sortByColumn(column.Header as string);
          }
          shouldSortColumn = true;
        },
      })}
      getResizerProps={(finalState, none, column, ctx) => ({
        onMouseDown: (e: React.MouseEvent) => {
          shouldSortColumn = false;
          ctx.resizeColumnStart(e, column, false);
        },
      })}
      onPageChange={changePage}
      pages={count ? Math.ceil(count / filterState.limit) : 1}
      showPagination={count ? count > 10 : false}
      onPageSizeChange={changePageSize}
      SubComponent={logRow => {
        const finalIndex = logRow.index;
        const finalRow = rows[finalIndex];
        const currentPayload = JSON.stringify(finalRow.request, null, 4);
        const finalResponse = JSON.stringify(finalRow.response, null, 4);
        return (
          <InvocationLogDetails
            requestPayload={currentPayload}
            responsePayload={finalResponse}
          />
        );
      }}
    />
  );
};

export default InvocationLogsTable;
