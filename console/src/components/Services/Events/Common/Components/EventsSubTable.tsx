import React from 'react';
import ReactTable from 'react-table';
import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';
import { Event } from '../../types';

type Props = {
  rows: any[];
  rowsFormatted: {
    request: any;
    response: any;
  }[];
  headings: {
    Header: string;
    accessor: string;
  }[];
  event: Event;
};

const EventsSubTable: React.FC<Props> = ({
  event,
  rows,
  rowsFormatted,
  headings,
}) => {
  return (
    <div className={styles.addPadding20Px}>
      {event.webhook_conf && (
        <div className={`row ${styles.add_mar_bottom_mid}`}>
          <div className="col-md-2">
            <b>Webhook:</b>
          </div>
          <div className="col-md-4">{event.webhook_conf}</div>
        </div>
      )}
      {event.comment && (
        <div className={`row ${styles.add_mar_bottom_mid}`}>
          <div className="col-md-2">
            <b>Comment:</b>
          </div>
          <div className="col-md-4">{event.comment}</div>
        </div>
      )}
      <div className={styles.add_mar_bottom_mid}>
        <b>Recent Invocations:</b>
      </div>
      <div className={`${styles.invocationsSection}`}>
        {rows.length ? (
          <ReactTable
            data={rowsFormatted}
            columns={headings}
            defaultPageSize={rows.length}
            minRows={0}
            showPagination={false}
            SubComponent={(logRow: any) => {
              const invocationLog = rows[logRow.index];
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
          <div>No data available</div>
        )}
      </div>
      <br />
      <br />
    </div>
  );
};

export default EventsSubTable;
