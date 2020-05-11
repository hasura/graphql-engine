import React from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';
import { ScheduledTrigger } from '../../Types';
import InvocationLogDetails from '../../Common/Components/InvocationLogDetails';
import Spinner from '../../../../Common/Spinner/Spinner';
import Button from '../../../../Common/Button/Button';

import styles from '../ScheduledTriggers.scss';
import {
  useInvocationLogs,
  invocationLogsColumns,
} from '../../Common/Hooks/useInvocationLogs';
import { getFetchInvocationLogsQuery } from '../../../../Common/utils/v1QueryUtils';

type Props = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
};

const Logs: React.FC<Props> = props => {
  const { dispatch, currentTrigger } = props;

  const {
    logs,
    error,
    loading,
    fetchLogs,
    oldLogsExist,
    newLogsExist,
  } = useInvocationLogs(
    currentTrigger ? currentTrigger.name : '',
    getFetchInvocationLogsQuery,
    dispatch
  );

  const loadOlderLogs = () => {
    fetchLogs('older');
  };

  const loadNewerLogs = () => {
    fetchLogs('newer');
  };

  if (error) {
    return (
      <div>
        Error fetching logs.{' '}
        <a href="#" onClick={loadOlderLogs} className={styles.cursorPointer}>
          Try again
        </a>
        .
      </div>
    );
  }

  // Get the headings
  const tableHeadings = [];
  const gridHeadings: any = []; // TODO

  invocationLogsColumns.forEach(column => {
    tableHeadings.push(<th key={column}>{column}</th>);
    gridHeadings.push({
      Header: column,
      accessor: column,
    });
  });

  const rows = logs.map((l: any) => {
    const row: any = {};
    invocationLogsColumns.forEach(c => {
      if (typeof l[c] === 'object') {
        row[c] = <div>{JSON.stringify(l[c], null, 4)}</div>;
      } else {
        row[c] = <div>{l[c]}</div>;
      }
    });
    return row;
  });

  return (
    <div>
      {newLogsExist && (
        <div className={styles.loadNewer}>
          <Button
            size="sm"
            color="white"
            onClick={loadNewerLogs}
            disabled={loading}
          >
            {loading ? <Spinner /> : 'Load new'}
          </Button>
        </div>
      )}
      <div>
        <ReactTable
          data={rows}
          columns={gridHeadings}
          minRows={0}
          showPagination={false}
          pageSize={rows.length}
          SubComponent={logRow => {
            const finalIndex = logRow.index;
            const finalRow = logs[finalIndex];
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
      </div>
      {oldLogsExist && (
        <div className={styles.loadOlder}>
          <Button
            size="sm"
            color="white"
            onClick={loadOlderLogs}
            disabled={loading}
          >
            {loading ? <Spinner /> : 'Load more'}
          </Button>
        </div>
      )}
    </div>
  );
};

export default Logs;
