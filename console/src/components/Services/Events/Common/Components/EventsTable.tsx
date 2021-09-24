import React from 'react';
import ReactTable, {
  ComponentPropsGetterC,
  ComponentPropsGetter0,
} from 'react-table';
import 'react-table/react-table.css';

import { FilterTableProps, GridHeadingProps } from './types';
import { ordinalColSort } from '../../../Data/utils';
import styles from '../../Events.scss';
import EventsSubTable from './EventsSubTable';
import ExpanderButton from './ExpanderButton';
import { sanitiseRow } from '../../utils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { convertDateTimeToLocale } from '../../../../Common/utils/jsUtils';
import { getEventStatusIcon, getEventDeliveryIcon } from './utils';
import Button from '../../../../Common/Button';
import { SupportedEvents } from '../../../../../metadata/queryUtils';

type CancelButtonProps = {
  id: string;
  onClickHandler: (e: React.MouseEvent) => void;
};

const CancelEventButton: React.FC<CancelButtonProps> = ({
  id,
  onClickHandler,
}) => (
  <Button
    key={id}
    className={styles.add_mar_right_small}
    onClick={onClickHandler}
    color="white"
    size="xs"
    title="Cancel Event"
    onMouseDown={e => {
      e.preventDefault();
    }}
  >
    <i className="fa fa-close" />
  </Button>
);

interface Props extends FilterTableProps {
  onCancelEvent?: (
    id: string,
    scheduled_at: string | Date | number,
    onSuccess: () => void
  ) => void;
  triggerType?: SupportedEvents;
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
    triggerType,
  } = props;

  const [currentPage, setCurrentPage] = React.useState(0);
  const [pageSize, setPageSize] = React.useState(filterState.limit ?? 10);

  if (!rows.length) {
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
      setCurrentPage(page);
      runQuery({
        offset: page * filterState.limit,
      });
    }
  };

  const changePageSize = (size: number) => {
    if (filterState.limit !== size) {
      setPageSize(size);
      runQuery({
        limit: size,
      });
    }
  };

  const onCancelHandler = (
    id: string,
    scheduled_time: string | Date | number
  ) => {
    if (onCancelEvent && triggerType) {
      onCancelEvent(id, scheduled_time, runQuery);
    }
  };

  const expanderActions: GridHeadingProps = {
    expander: true,
    Header: '',
    accessor: 'expander',
    Expander: ({ isExpanded, viewIndex }) => {
      const row = rows[viewIndex];
      return (
        <>
          {columns.includes('actions') && (
            <CancelEventButton
              id={row.id}
              onClickHandler={event => {
                onCancelHandler(row.id, row.scheduled_time);
                event.stopPropagation();
              }}
            />
          )}
          <ExpanderButton isExpanded={isExpanded} />
        </>
      );
    },
  };

  const gridHeadings = [expanderActions];

  sortedColumns.forEach(column => {
    if (column !== 'actions') {
      gridHeadings.push({
        Header: column,
        accessor: column,
      });
    }
  });

  const invocationColumns = ['status', 'id', 'created_at'];
  const invocationDataTriggerColumns = ['status', 'id', 'created_at'];

  const invocationGridHeadings: GridHeadingProps[] = [expanderActions];
  const addToGridHeadings = (headAccArr: string[]) => {
    headAccArr.forEach(column => {
      invocationGridHeadings.push({
        Header: column,
        accessor: column,
      });
    });
  };

  if (triggerType) {
    addToGridHeadings(invocationColumns);
  } else {
    addToGridHeadings(invocationDataTriggerColumns);
  }

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

  const getNumOfPages = (
    currentPageSize: number,
    currentCount: number | undefined,
    currentRowData: Record<string, any>[]
  ) => {
    if (currentCount) {
      return Math.ceil(currentCount / currentPageSize);
    }
    return Math.ceil(currentRowData.length / currentPageSize);
  };

  return (
    <ReactTable
      className="-highlight"
      data-test="events-table"
      data={rowsFormatted}
      columns={gridHeadings}
      resizable
      manual
      onPageChange={changePage}
      page={currentPage}
      pages={getNumOfPages(pageSize, count, rowsFormatted)}
      showPagination={count ? count > 10 : false}
      onPageSizeChange={changePageSize}
      sortable={false}
      minRows={0}
      getTheadThProps={getTheadThProps}
      getResizerProps={getResizerProps}
      defaultPageSize={10}
      SubComponent={row => {
        const currentRow = rows[row.index];
        if (triggerType) {
          return (
            <EventsSubTable
              event={currentRow}
              rows={[]}
              rowsFormatted={[]}
              headings={invocationGridHeadings}
              makeAPICall
              triggerType={triggerType}
            />
          );
        }
        const logs = [currentRow];
        const invocationRows = logs.map((r: any) => {
          const newRow: Record<string, JSX.Element> = {};
          // Insert cells corresponding to all rows
          invocationDataTriggerColumns.forEach(col => {
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
