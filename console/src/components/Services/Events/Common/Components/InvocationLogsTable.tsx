import React from 'react';
import ReactTable, {
  ComponentPropsGetterC,
  ComponentPropsGetter0,
} from 'react-table';
import 'react-table/react-table.css';
import { FilterTableProps } from './types';
import { Dispatch } from '../../../../../types';
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

interface Props extends FilterTableProps {
  dispatch: Dispatch;
}

const InvocationLogsTable: React.FC<Props> = props => {
  const { rows, filterState, runQuery, columns, count, dispatch } = props;
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
      data={rowsFormatted}
      columns={gridHeadings}
      minRows={0}
      resizable
      manual
      getTheadThProps={getTheadThProps}
      getResizerProps={getResizerProps}
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
