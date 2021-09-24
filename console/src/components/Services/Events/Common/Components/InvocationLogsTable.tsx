import React from 'react';
import ReactTable, {
  ComponentPropsGetterC,
  ComponentPropsGetter0,
} from 'react-table';
import 'react-table/react-table.css';
import { FilterTableProps, GridHeadingProps } from './types';
import { Dispatch } from '../../../../../types';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { ordinalColSort } from '../../../Data/utils';
import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';
import ExpanderButton from './ExpanderButton';
import { redeliverDataEvent } from '../../ServerIO';
import ReloadIcon from '../../../../Common/Icons/Reload';
import Modal from '../../../../Common/Modal/Modal';
import RedeliverEvent from './RedeliverEvent';
import { convertDateTimeToLocale } from '../../../../Common/utils/jsUtils';
import { Nullable } from '../../../../Common/utils/tsUtils';
import { getInvocationLogStatus } from './utils';
import Button from '../../../../Common/Button/Button';
import { QualifiedTable } from '../../../../../metadata/types';

type RedeliverButtonProps = {
  onClickHandler: (e: React.MouseEvent) => void;
};

const RedliverEventButton: React.FC<RedeliverButtonProps> = ({
  onClickHandler,
}) => (
  <Button
    color="white"
    size="xs"
    title="Redeliver event"
    onClick={onClickHandler}
    className={`${styles.cursorPointer} ${styles.add_mar_right_small}`}
  >
    <ReloadIcon className="" />
  </Button>
);

interface Props extends FilterTableProps {
  dispatch: Dispatch;
  tableDef?: QualifiedTable;
  tableSource?: string;
}

const InvocationLogsTable: React.FC<Props> = props => {
  const { rows, filterState, runQuery, columns, count, dispatch } = props;
  const [redeliveredEventId, setRedeliveredEventId] = React.useState<
    Nullable<string>
  >(null);
  const [isRedelivering, setIsRedelivering] = React.useState(false);
  const [currentPage, setPage] = React.useState(0);
  const [pageSize, setPageSize] = React.useState(10);

  const redeliverHandler = (
    eventId: string,
    tableDef?: QualifiedTable,
    tableSource?: string
  ) => {
    if (!tableDef || !tableSource) {
      return;
    }
    setIsRedelivering(true);
    dispatch(
      redeliverDataEvent(
        eventId,
        tableDef,
        tableSource,
        () => {
          setRedeliveredEventId(eventId);
          setIsRedelivering(false);
        },
        () => {
          setIsRedelivering(true);
        }
      )
    );
  };

  const redeliverModal = (eventId: string) => {
    if (!redeliveredEventId) return null;
    return (
      <Modal
        title={<div>Redeliver Event</div>}
        show={eventId === redeliveredEventId}
        onClose={() => setRedeliveredEventId(null)}
        onCancel={() => setRedeliveredEventId(null)}
        customClass={styles.redeliverModal}
      >
        <RedeliverEvent
          eventId={redeliveredEventId}
          dispatch={dispatch}
          eventDataSource={props.tableSource ?? ''}
        />
      </Modal>
    );
  };

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
      setPage(page);
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

  const expanderActions: GridHeadingProps = {
    expander: true,
    Header: '',
    accessor: 'expander',
    Expander: ({ isExpanded, viewIndex }) => {
      const row = rows[viewIndex];
      return (
        <>
          {columns.includes('redeliver') && (
            <RedliverEventButton
              onClickHandler={e => {
                if (isRedelivering) {
                  return;
                }
                redeliverHandler(
                  row.event_id,
                  props.tableDef,
                  props.tableSource
                );
                e.stopPropagation();
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
    if (column !== 'redeliver') {
      gridHeadings.push({
        Header: column,
        accessor: column,
      });
    }
  });

  const rowsFormatted = rows.map(r => {
    let formattedRow: any = {};
    Object.keys(r).forEach((col: string) => {
      formattedRow[col] = <div>{r[col]}</div>;
    });
    formattedRow = {
      ...formattedRow,
      status: <div>{getInvocationLogStatus(r.status)}</div>,
      created_at: r.created_at && (
        <div>{convertDateTimeToLocale(r.created_at)}</div>
      ),
      id: (
        <div>
          {r.id}
          {columns.includes('redeliver') ? redeliverModal(r.event_id) : null}
        </div>
      ),
    };
    return formattedRow;
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
      data={rowsFormatted}
      columns={gridHeadings}
      minRows={0}
      resizable
      defaultPageSize={rowsFormatted.length}
      manual
      getTheadThProps={getTheadThProps}
      getResizerProps={getResizerProps}
      onPageChange={changePage}
      page={currentPage}
      pages={getNumOfPages(pageSize, count, rowsFormatted)}
      showPagination={count ? count > 10 : false}
      onPageSizeChange={changePageSize}
      SubComponent={logRow => {
        const finalIndex = logRow.index;
        const finalRow = rows[finalIndex];
        const currentPayload = JSON.stringify(finalRow?.request ?? {}, null, 4);
        const finalResponse = JSON.stringify(finalRow?.response ?? {}, null, 4);
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
