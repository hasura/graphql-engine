import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import ReactTable from 'react-table';
import InvocationLogDetails from './InvocationLogDetails';
import { Event } from '../../types';
import {
  SupportedEvents,
  getEventInvocationsLogByID,
  getEventTriggerInvocationByID,
} from '../../../../../metadata/queryUtils';
import { sanitiseRow } from '../../utils';
import { Dispatch, ReduxState } from '../../../../../types';
import requestAction from '../../../../../utils/requestAction';
import Endpoints from '../../../../../Endpoints';
import Spinner from '../../../../Common/Spinner/Spinner';
import { currentDriver } from '../../../../../dataSources';

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

const invocationColumns = ['http_status', 'id', 'created_at'];

const RenderEventSubTable: React.FC<RenderSubTableProps> = ({
  event,
  rows,
  rowsFormatted,
  headings,
}) => (
  <div className="p-md">
    {event.webhook_conf && (
      <div className="row mb-sm">
        <div className="w-1/6">
          <b>Webhook:</b>
        </div>
        <div className="w-1/3">{event.webhook_conf}</div>
      </div>
    )}
    {event.comment && (
      <div className="row mb-xs">
        <div className="w-1/6">
          <b>Comment:</b>
        </div>
        <div className="w-4/6">{event.comment}</div>
      </div>
    )}
    <div className="mb-xs">
      <b>Recent Invocations:</b>
    </div>
    <div>
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
      const url = Endpoints.metadata;
      const source_prefix = currentDriver === 'postgres' ? 'pg' : currentDriver;
      const payload = getEventTriggerInvocationByID(
        source_prefix,
        props.event.id,
        props.source
      );
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
      <div className="p-md">
        <div className="pb-xs">
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
        <div className="text-center overflow-hidden" key={`${col}-${col}-${i}`}>
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
  source: state.tables.currentDataSource,
});
const connector = connect(mapStateToProps, mapDispatchToProps);
type InjectedReduxProps = ConnectedProps<typeof connector>;
const ConnectedEventSubTable = connector(EventsSubTable);

export default ConnectedEventSubTable;
