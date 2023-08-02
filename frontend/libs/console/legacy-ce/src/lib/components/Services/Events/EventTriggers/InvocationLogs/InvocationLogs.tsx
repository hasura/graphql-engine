import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
  makeRelationshipFilter,
} from '../../../../Common/FilterQuery/types';
import { etInvocationLogsTable } from '../utils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import TableHeader from '../TableCommon/TableHeader';
import InvocationLogsTable from '../../Common/Components/InvocationLogsTable';
import {
  connector,
  EventsLogsInjectedProps,
} from '../Common/eventLogsMapStateToProps';

type Props = EventsLogsInjectedProps;

const InvocationLogs: React.FC<Props> = props => {
  const { dispatch, triggerName, currentTrigger, readOnlyMode } = props;
  const renderRows: FilterRenderProp = (
    rows,

    filterState,
    setFilterState,
    runQuery
  ) => (
    <div className="bootstrap-jail">
      <InvocationLogsTable
        rows={rows}
        filterState={filterState}
        setFilterState={setFilterState}
        runQuery={runQuery}
        columns={['id', 'redeliver', 'status', 'event_id', 'created_at']}
        identifier={triggerName}
        dispatch={dispatch}
        tableDef={{
          name: currentTrigger.table_name,
          schema: currentTrigger.schema_name,
        }}
        tableSource={currentTrigger.source}
      />
    </div>
  );

  return (
    <React.Fragment>
      <TableHeader
        count={null}
        triggerName={triggerName}
        tabName="logs"
        readOnlyMode={readOnlyMode}
      />
      <br />
      <FilterQuery
        table={etInvocationLogsTable}
        dispatch={dispatch}
        render={renderRows}
        relationships={['event']}
        presets={{
          sorts: [makeOrderBy('created_at', 'desc')],
          filters: [
            makeRelationshipFilter(
              'event',
              makeValueFilter('trigger_name', '$eq', triggerName)
            ),
          ],
        }}
        triggerName={triggerName}
        triggerOp="invocation"
        triggerType="data"
        currentSource={currentTrigger.source}
      />
    </React.Fragment>
  );
};
const InvocationLogsConnector = connector(InvocationLogs);
export default InvocationLogsConnector;
