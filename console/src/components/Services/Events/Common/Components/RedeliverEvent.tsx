import React from 'react';
import ReactTable from 'react-table';
import Spinner from '../../../../Common/Spinner/Spinner';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import InvocationLogDetails from '../../Common/Components/InvocationLogDetails';
import { getEventLogs } from '../../ServerIO';
import { InvocationLog } from '../../types';
import styles from '../../Events.scss';
import { Dispatch } from '../../../../../types';

type Props = {
  eventId: string;
  dispatch: Dispatch;
};

const RedeliverEvent: React.FC<Props> = ({ dispatch, eventId }) => {
  const [error, setError] = React.useState<null | Error>(null);
  const [logs, setLogs] = React.useState<InvocationLog[]>([]);

  React.useEffect(() => {
    const intervalId = setInterval(() => {
      dispatch(
        getEventLogs(
          eventId,
          'data',
          l => {
            setLogs(l);
          },
          e => {
            setError(e);
          }
        )
      );
    }, 2000);
    return () => {
      clearInterval(intervalId);
    };
  }, []);

  const gridHeadings = ['status', 'id', 'created_at'].map(column => {
    return {
      Header: column,
      accessor: column,
    };
  });

  const latestLog = logs[0];

  if (!logs.length) {
    return <Spinner />;
  }

  return (
    <div className="content-fluid">
      <div>
        <div
          className={`${styles.padd_left_remove} col-md-12 ${styles.padd_right_remove}`}
        >
          <div className={`${styles.padd_left_remove} col-md-6`}>
            <div> Request </div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(latestLog.request, null, 4)}
              minLines={10}
              maxLines={30}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              style={{ backgroundColor: '#fdf9ed', marginTop: '10px' }}
            />
          </div>
          <div className={`${styles.padd_right_remove} col-md-6`}>
            <div className={styles.display_flex}>
              <div className={styles.add_mar_right}>
                Latest Invocation Response
              </div>
            </div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(
                error === null ? latestLog.response : error,
                null,
                4
              )}
              minLines={8}
              maxLines={30}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              style={{
                backgroundColor: '#fdf9ed',
                marginTop: '10px',
              }}
            />
          </div>
        </div>
        <div
          className={`${styles.redeliverEventSection} ${styles.padd_top} redeliverEventSection`}
        >
          <div className={styles.add_mar_bottom}>
            <em>Recent Invocations</em>
          </div>
          <ReactTable
            data={logs}
            columns={gridHeadings}
            minRows={0}
            resizable
            manual
            showPagination={false}
            freezeWhenExpanded
            SubComponent={(logRow: any) => {
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
      </div>
    </div>
  );
};

export default RedeliverEvent;
