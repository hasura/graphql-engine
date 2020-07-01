import React from 'react';
import ReactTable, {
  ComponentPropsGetterC,
  ComponentPropsGetter0,
} from 'react-table';
import 'react-table/react-table.css';
import { FilterTableProps } from './types';
import { ordinalColSort } from '../../../Data/utils';
import styles from '../../Events.scss';
import EventsSubTable from './EventsSubTable';
import { sanitiseRow } from '../../utils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { convertDateTimeToLocale } from '../../../../Common/utils/jsUtils';
import { getEventStatusIcon, getEventDeliveryIcon } from './utils';
import Button from '../../../../Common/Button';

type CancelButtonProps = {
  id: string;
  onClickHandler: () => void;
};

const CancelEventButton: React.FC<CancelButtonProps> = ({
  id,
  onClickHandler,
}) => (
  <Button
    key={id}
    onClick={() => onClickHandler()}
    color="white"
    size="xs"
    title="Cancel Event"
  >
    <i className="fa fa-close" />
  </Button>
);

interface Props extends FilterTableProps {
  onCancelEvent?: (id: string, onSuccess: () => void) => void;
}

const EventsTable: React.FC<Props> = props => {
  const {
    rows,
    count,
    filterState,
    runQuery,
    columns,
    identifier,
    onCancelEvent,
  } = props;

  if (rows.length === 0) {
    return <div className={styles.add_mar_top}>No data available</div>;
  }

  const sortedColumns = columns.sort(ordinalColSort);

  let shouldSortColumn = true;

  const sortByColumn = (col: string) => {
    // Remove all the existing order_bys

    const existingColSort = filterState.sorts.find(s => s.column === col);
    if (existingColSort && existingColSort.type === 'asc') {
      runQuery({
        sorts: [makeOrderBy(col, 'desc')],
      });
    } else {
      runQuery({
        sorts: [makeOrderBy(col, 'asc')],
      });
    }
  };

  const changePage = (page: number) => {
    if (filterState.offset !== page * filterState.limit) {
      runQuery({
        offset: page * filterState.limit,
      });
    }
  };

  const changePageSize = (size: number) => {
    if (filterState.limit !== size) {
      runQuery({
        limit: size,
      });
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

  const onCancelHandler = (id: string) => {
    if (onCancelEvent) {
      onCancelEvent(id, runQuery);
    }
  };

  const rowsFormatted = rows.map(row => {
    const formattedRow = Object.keys(row).reduce((fr, col) => {
      return {
        ...fr,
        [col]: <div>{row[col]}</div>,
      };
    }, {});
    return {
      ...formattedRow,
      delivered: getEventDeliveryIcon(row.delivered),
      status: getEventStatusIcon(row.status),
      scheduled_time: row.scheduled_time ? (
        <div>{convertDateTimeToLocale(row.scheduled_time)}</div>
      ) : undefined,
      created_at: row.created_at ? (
        <div>{convertDateTimeToLocale(row.created_at)}</div>
      ) : undefined,
      actions: columns.includes('actions') ? (
        <CancelEventButton
          id={row.id}
          onClickHandler={() => onCancelHandler(row.id)}
        />
      ) : undefined,
    };
  });

  const getTheadThProps: ComponentPropsGetterC = (
    finalState,
    some,
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
      data-test="events-table"
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
          <EventsSubTable
            event={currentRow}
            rows={logs}
            rowsFormatted={invocationRows}
            headings={invocationGridHeadings}
          />
        );
      }}
    />
  );
};

export default EventsTable;
