import React from 'react';
import ReactTable from 'react-table';
import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';

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
};

const EventsSubTable: React.FC<Props> = ({ rows, rowsFormatted, headings }) => {
  return (
    <div className={styles.add_padding20}>
      <em>Recent Invocations</em>
      <div className={`${styles.invocationsSection} invocationsSection`}>
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
          <div className={styles.add_mar_top}>No data available</div>
        )}
      </div>
      <br />
      <br />
    </div>
  );
};
export default EventsSubTable;
