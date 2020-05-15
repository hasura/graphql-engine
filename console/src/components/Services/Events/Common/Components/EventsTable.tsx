import React from 'react';
import ReactTable, {
  ComponentPropsGetterC,
  ComponentPropsGetter0,
} from 'react-table';
import 'react-table/react-table.css';
import { FilterTableProps } from './types';
import CheckIcon from '../../../../Common/Icons/Check';
import CrossIcon from '../../../../Common/Icons/Cross';
import ClockIcon from '../../../../Common/Icons/Clock';
import SkullIcon from '../../../../Common/Icons/Skull';
import { ordinalColSort } from '../../../Data/utils';
import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';
import { sanitiseRow } from '../../utils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { convertDateTimeToLocale } from '../../../../Common/utils/jsUtils';

type Props = FilterTableProps;

const EventsTable: React.FC<Props> = props => {
  const { rows, count, filterState, runQuery, columns, identifier } = props;

  if (rows.length === 0) {
    return <div className={styles.add_mar_top}>No data available</div>;
  }

  const sortedColumns = columns.sort(ordinalColSort);

  let shouldSortColumn = true;

  const sortByColumn = (col: string) => {
    // Remove all the existing order_bys

    const existingColSort = filterState.sorts.find(s => s.column === col);
    if (existingColSort && existingColSort.type === 'asc') {
      runQuery(undefined, undefined, [makeOrderBy(col, 'desc')]);
    } else {
      runQuery(undefined, undefined, [makeOrderBy(col, 'asc')]);
    }
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
    const formattedRow = Object.keys(r).reduce((fr, col) => {
      return {
        ...fr,
        [col]: <div>{r[col]}</div>,
      };
    }, {});
    return {
      ...formattedRow,
      delivered: r.delivered ? (
        <CheckIcon className="" />
      ) : (
        <CrossIcon className="" />
      ),
      status:
        r.status === 'scheduled' ? (
          <ClockIcon className="" />
        ) : r.status === 'dead' ? (
          <SkullIcon className="" />
        ) : (
          <CheckIcon className="" />
        ),
      scheduled_at: (
        <div>
          {r.scheduled_time
            ? convertDateTimeToLocale(r.scheduled_time, false)
            : undefined}
        </div>
      ),
      created_at: (
        <div>
          {r.created_at
            ? convertDateTimeToLocale(r.created_at, true)
            : undefined}
        </div>
      ),
    };
  });

  const getTheadThProps: ComponentPropsGetterC = (
    finalState: any,
    some: any,
    column
  ) => ({
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
  });

  const getResizerProps: ComponentPropsGetter0 = (
    finalState,
    none,
    column,
    ctx
  ) => ({
    onMouseDown: (e: React.MouseEvent) => {
      shouldSortColumn = false;
      ctx.resizeColumnStart(e, column, false);
    },
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
      getTheadThProps={getTheadThProps}
      getResizerProps={getResizerProps}
      defaultPageSize={10}
      SubComponent={row => {
        const currentRow = rows[row.index];
        const logs =
          currentRow.cron_event_logs ||
          currentRow.scheduled_event_logs ||
          currentRow.logs ||
          [];
        const invocationRows = logs.map((r: any) => {
          const newRow: Record<string, JSX.Element> = {};
          // Insert cells corresponding to all rows
          invocationColumns.forEach(col => {
            newRow[col] = (
              <div
                className={styles.tableCellCenterAlignedOverflow}
                key={`${identifier}-${col}-${row.index}`}
              >
                {sanitiseRow(col, r)}
              </div>
            );
          });
          return newRow;
        });
        return (
          <div className={styles.add_padding20}>
            <em>Recent Invocations</em>
            <div className={`${styles.invocationsSection} invocationsSection`}>
              {invocationRows.length ? (
                <ReactTable
                  data={invocationRows}
                  columns={invocationGridHeadings}
                  defaultPageSize={invocationRows.length}
                  minRows={0}
                  showPagination={false}
                  SubComponent={logRow => {
                    const invocationLog = logs[logRow.index];
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
