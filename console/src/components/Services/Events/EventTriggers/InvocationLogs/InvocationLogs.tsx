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

interface Props extends EventsLogsInjectedProps {}

const InvocationLogs: React.FC<Props> = props => {
  const {
    dispatch,
    triggerName,
    readOnlyMode,
    currentSource,
    currentTrigger,
  } = props;

  const renderRows: FilterRenderProp = (
    rows,
    count,
    filterState,
    setFilterState,
    runQuery
  ) => (
    <InvocationLogsTable
      rows={rows}
      filterState={filterState}
      count={count}
      setFilterState={setFilterState}
      runQuery={runQuery}
      columns={['id', 'redeliver', 'status', 'event_id', 'created_at']}
      identifier={triggerName}
      dispatch={dispatch}
      tableDef={{
        name: currentTrigger.table_name,
        schema: currentTrigger.schema_name,
      }}
      tableSource={currentSource}
    />
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
        currentSource={currentSource}
      />
    </React.Fragment>
  );
};
const InvocationLogsConnector = connector(InvocationLogs);
export default InvocationLogsConnector;
