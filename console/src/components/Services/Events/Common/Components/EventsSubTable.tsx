import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import ReactTable from 'react-table';

import styles from '../../Events.scss';
import InvocationLogDetails from './InvocationLogDetails';
import { Event } from '../../types';
import {
  SupportedEvents,
  getEventInvocationsLogByID,
} from '../../../../../metadata/queryUtils';
import { sanitiseRow } from '../../utils';
import { Dispatch } from '../../../../../types';
import requestAction from '../../../../../utils/requestAction';
import Endpoints from '../../../../../Endpoints';

interface Props extends InjectedReduxProps {
  rows: any[];
  rowsFormatted: any[];
  headings: {
    Header: string;
    accessor: string;
  }[];
  event: Event;
  makeAPICall?: boolean;
  triggerType?: SupportedEvents;
}

type RenderSubTableProps = Omit<
  Props,
  'makeAPICall' | 'triggerType' | 'getEventInvocationData'
>;

const invocationColumns = ['status', 'id', 'created_at'];

const RenderEventSubTable: React.FC<RenderSubTableProps> = ({
  event,
  rows,
  rowsFormatted,
  headings,
}) => (
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

const EventsSubTable: React.FC<Props> = ({
  makeAPICall,
  triggerType,
  ...props
}) => {
  const [inv, setInvocations] = React.useState([]);
  const [errInfo, setErrInfo] = React.useState(null);

  React.useEffect(() => {
    if (!triggerType || !props.event.id) {
      return;
    }
    // TODO: handle a "loading" state
    const url = Endpoints.metadata;
    const payload = getEventInvocationsLogByID(triggerType, props.event.id);
    const options = {
      method: 'POST',
      body: JSON.stringify(payload),
    };
    props
      .getEventInvocationData(url, options)
      .then(data => {
        if (data && data?.invocations && !data.error) {
          setInvocations(data.invocations);
          return;
        }
        setErrInfo(data.error);
      })
      .catch(err => setErrInfo(err));
  }, []);

  if (!makeAPICall || !triggerType) {
    return (
      <RenderEventSubTable
        event={props.event}
        rowsFormatted={props.rowsFormatted}
        headings={props.headings}
        rows={props.rows}
      />
    );
  }

  if (errInfo) {
    return (
      <div>
        <br />
        Error occurred in fetching information about recent invocations.
        <br />
      </div>
    );
  }

  const invocationRows = inv.map((r: any, i: number) => {
    const newRow: Record<string, JSX.Element> = {};
    // Insert cells corresponding to all rows
    invocationColumns.forEach(col => {
      newRow[col] = (
        <div
          className={styles.tableCellCenterAlignedOverflow}
          key={`${col}-${col}-${i}`}
        >
          {sanitiseRow(col, r)}
        </div>
      );
    });
    return newRow;
  });

  return (
    <RenderEventSubTable
      event={props.event}
      rows={inv}
      rowsFormatted={invocationRows}
      headings={props.headings}
    />
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  getEventInvocationData: (url: string, options: RequestInit) =>
    dispatch(requestAction(url, options)),
});
const connector = connect(null, mapDispatchToProps);
type InjectedReduxProps = ConnectedProps<typeof connector>;
const ConnectedEventSubTable = connector(EventsSubTable);

export default ConnectedEventSubTable;
