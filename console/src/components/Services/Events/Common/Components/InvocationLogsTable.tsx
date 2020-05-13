import React from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';
import { FilterTableProps } from './Types';
// import { convertDateTimeToLocale } from '../../../../Common/utils/jsUtils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { ordinalColSort } from '../../../Data/utils';
import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';
import { redeliverDataEvent } from '../../ServerIO';
import ReloadIcon from '../../../../Common/Icons/Reload';
import CheckIcon from '../../../../Common/Icons/Check';
import CrossIcon from '../../../../Common/Icons/Cross';
import Modal from '../../../../Common/Modal/Modal';
import RedeliverEvent from './RedeliverEvent';
import { convertDateTimeToLocale } from '../../../../Common/utils/jsUtils';
import { Nullable } from '../../../../Common/utils/tsUtils';

type Props = FilterTableProps & {
  dispatch: any;
};

const InvocationLogsTable: React.FC<Props> = props => {
  const {
    rows,
    filterState,
    setFilterState,
    runQuery,
    columns,
    count,
    dispatch,
  } = props;
  const [redeliveredEventId, setRedeliveredEventId] = React.useState<
    Nullable<string>
  >(null);
  const [isRedelivering, setIsRedelivering] = React.useState(false);

  const redeliverHandler = (eventId: string) => {
    setIsRedelivering(true);
    dispatch(
      redeliverDataEvent(
        eventId,
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
        <RedeliverEvent eventId={redeliveredEventId} dispatch={dispatch} />
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
      setFilterState.sorts([makeOrderBy(col, 'desc')]);
    } else {
      setFilterState.sorts([makeOrderBy(col, 'asc')]);
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

  const rowsFormatted = rows.map(r => {
    let formattedRow: any = {};
    Object.keys(r).forEach((col: string) => {
      formattedRow[col] = <div>{r[col]}</div>;
    });
    formattedRow = {
      ...formattedRow,
      status: (
        <div>
          {r.status < 300 ? (
            <CheckIcon className="" />
          ) : (
            <CrossIcon className="" />
          )}
        </div>
      ),
      created_at: r.created_at && (
        <div>{convertDateTimeToLocale(r.created_at, true)}</div>
      ),
      id: (
        <div>
          {r.id}
          {columns.includes('redeliver') ? redeliverModal(r.event_id) : null}
        </div>
      ),
    };
    if (columns.includes('redeliver')) {
      formattedRow.redeliver = (
        <div
          onClick={() => {
            if (isRedelivering) {
              return;
            }
            redeliverHandler(r.event_id);
          }}
          className={styles.cursorPointer}
        >
          <ReloadIcon className="" />
        </div>
      );
    }
    return formattedRow;
  });

  return (
    <ReactTable
      data={rowsFormatted}
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
