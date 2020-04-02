import React from 'react';
import { ScheduledTrigger } from '../../Types';
import ReactTable from 'react-table';
import AceEditor from 'react-ace';
import 'brace/mode/json';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
import 'react-table/react-table.css';
import Spinner from '../../../../Common/Spinner/Spinner';
import Button from '../../../../Common/Button/Button';

import styles from '../ScheduledTriggers.scss';
import { useInvocationLogs, invocationLogsColumns } from './utils';
import { getFetchInvocationLogsQuery } from '../../../../Common/utils/v1QueryUtils';

type LogsProps = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
};

const Logs = (props: LogsProps) => {
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

  invocationLogsColumns.forEach((column, i) => {
    tableHeadings.push(<th key={i}>{column}</th>);
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
              <div style={{ padding: '20px' }}>
                <Tabs
                  animation={false}
                  defaultActiveKey={1}
                  id="requestResponseTab"
                >
                  <Tab eventKey={1} title="Request">
                    <div className={styles.add_mar_top}>
                      <div className={styles.subheading_text}>Request</div>
                      <AceEditor
                        mode="json"
                        theme="github"
                        name="payload"
                        value={currentPayload}
                        minLines={4}
                        maxLines={100}
                        width="100%"
                        showPrintMargin={false}
                        showGutter={false}
                      />
                    </div>
                  </Tab>
                  <Tab eventKey={2} title="Response">
                    <div className={styles.add_mar_top}>
                      <div className={styles.subheading_text}>Response</div>
                      <AceEditor
                        mode="json"
                        theme="github"
                        name="response"
                        value={finalResponse}
                        minLines={4}
                        maxLines={100}
                        width="100%"
                        showPrintMargin={false}
                        showGutter={false}
                      />
                    </div>
                  </Tab>
                </Tabs>
              </div>
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
