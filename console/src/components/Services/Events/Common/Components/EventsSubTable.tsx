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
import { parseEventsSQLResp, sanitiseRow } from '../../utils';
import { Dispatch, ReduxState } from '../../../../../types';
import requestAction from '../../../../../utils/requestAction';
import Endpoints from '../../../../../Endpoints';
import { getDataTriggerInvocations } from '../../../../../metadata/metadataTableUtils';
import Spinner from '../../../../Common/Spinner/Spinner';

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
  const [inv, setInvocations] = React.useState<
    Record<string, string | number | boolean>[]
  >([]);
  const [errInfo, setErrInfo] = React.useState(null);
  const [loading, setLoading] = React.useState<boolean>(false);

  React.useEffect(() => {
    if (!triggerType || !props.event.id) {
      return;
    }
    if (triggerType === 'data' && props.event.id) {
      const url = Endpoints.query;
      const payload = getDataTriggerInvocations(props.event.id);
      const options = {
        method: 'POST',
        body: JSON.stringify(payload),
        headers: props.headers,
      };
      setLoading(true);
      props
        .getEventInvocationData(url, options)
        .then(data => {
          setLoading(false);
          const parsed = parseEventsSQLResp(data?.result);
          if (parsed) setInvocations(parsed);
          if (data && data?.invocations && !data.error) {
            setInvocations(data.invocations);
            return;
          }
          setErrInfo(data.error);
        })
        .catch(err => {
          setErrInfo(err);
          setLoading(false);
        });
      return;
    }
    const url = Endpoints.metadata;
    const payload = getEventInvocationsLogByID(triggerType, props.event.id);
    const options = {
      method: 'POST',
      body: JSON.stringify(payload),
      headers: props.headers,
    };
    setLoading(true);

    props
      .getEventInvocationData(url, options)
      .then(data => {
        setLoading(false);
        if (data && data?.invocations && !data.error) {
          setInvocations(data.invocations);
          return;
        }
        setErrInfo(data.error);
      })
      .catch(err => {
        setLoading(false);
        setErrInfo(err);
      });
  }, []);

  if (loading) {
    return (
      <div className={styles.addPadding20Px}>
        <div className={styles.add_mar_bottom_mid}>
          <b>Recent Invocations:</b>
        </div>
        <Spinner />
      </div>
    );
  }
  if (!makeAPICall || !triggerType) {
    return <RenderEventSubTable {...props} />;
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
      {...props}
      event={props.event}
      rows={inv}
      rowsFormatted={invocationRows}
      headings={props.headings}
    />
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  getEventInvocationData: (url: string, options: RequestInit) =>
    dispatch(requestAction(url, options, undefined, undefined, true, true)),
});

const mapStateToProps = (state: ReduxState) => ({
  headers: state.tables.dataHeaders,
});
const connector = connect(mapStateToProps, mapDispatchToProps);
type InjectedReduxProps = ConnectedProps<typeof connector>;
const ConnectedEventSubTable = connector(EventsSubTable);

export default ConnectedEventSubTable;
