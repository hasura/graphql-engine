import React from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';
import {
  FilterState,
  SetFilterState,
  RunQuery,
} from '../../../../Common/FilterQuery/utils';
import CheckIcon from '../../../../Common/Icons/Check';
import CrossIcon from '../../../../Common/Icons/Cross';
import { ordinalColSort } from '../../../Data/utils';
import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';
import { sanitiseRow } from '../../utils';

type Props = {
  rows: any[];
  count?: number;
  filterState: FilterState;
  setFilterState: SetFilterState;
  runQuery: RunQuery;
  columns: string[];
  triggerName: string;
};

const EventsTable = (props: Props) => {
  const {
    rows,
    count,
    filterState,
    setFilterState,
    runQuery,
    columns,
    triggerName,
  } = props;

  if (rows.length === 0) {
    return <div className={styles.add_mar_top}>No data available</div>;
  }

  const sortedColumns = columns.sort(ordinalColSort);

  let shouldSortColumn = true;

  const sortByColumn = (col: string) => {
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

  const invocationColumns = ['status', 'id', 'created_at'];

  const invocationGridHeadings = invocationColumns.map(column => {
    return {
      Header: column,
      accessor: column,
    };
  });
  const rowsFormatted = rows.map(r => {
    return {
      ...r,
      delivered: r.delivered ? (
        <CheckIcon className="" />
      ) : (
        <CrossIcon className="" />
      ),
    };
  });

  return (
    <ReactTable
      className="-highlight"
      data={rowsFormatted}
      columns={gridHeadings}
      resizable
      manual
      onPageChange={changePage}
      pages={count ? Math.ceil(count / filterState.limit) : 1}
      showPagination={count ? count > 10 : false}
      onPageSizeChange={changePageSize}
      sortable={false}
      minRows={0}
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
      defaultPageSize={10}
      SubComponent={row => {
        const currentRow = rows[row.index];
        const invocationRows = currentRow.logs.map((r: any) => {
          const newRow: Record<string, JSX.Element> = {};
          // Insert cells corresponding to all rows
          invocationColumns.forEach(col => {
            newRow[col] = (
              <div
                className={styles.tableCellCenterAlignedOverflow}
                key={`${triggerName}-${col}-${row.index}`}
              >
                {sanitiseRow(col, r)}
              </div>
            );
          });
          return newRow;
        });
        return (
          <div style={{ padding: '20px' }}>
            <em>Recent Invocations</em>
            <div className={`${styles.invocationsSection} invocationsSection`}>
              {invocationRows.length ? (
                <ReactTable
                  data={invocationRows}
                  columns={invocationGridHeadings}
                  defaultPageSize={currentRow.logs.length}
                  minRows={0}
                  showPagination={false}
                  SubComponent={logRow => {
                    const invocationLog = currentRow.logs[logRow.index];
                    const currentPayload = JSON.stringify(
                      invocationLog.request,
                      null,
                      4
                    );
                    const finalResponse = JSON.stringify(
                      invocationLog.response,
                      null,
                      4
                    );
                    return (
                      <InvocationLogDetails
                        requestPayload={currentPayload}
                        responsePayload={finalResponse}
                      />
                    );
                  }}
                />
              ) : (
                <div className={styles.add_mar_top}>No data available</div>
              )}
            </div>
            <br />
            <br />
          </div>
        );
      }}
    />
  );
};

export default EventsTable;
