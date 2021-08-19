import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
} from '../../../../Common/FilterQuery/types';
import { etEventsTable } from '../utils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import TableHeader from '../TableCommon/TableHeader';
import EventsTable from '../../Common/Components/EventsTable';
import {
  connector,
  EventsLogsInjectedProps,
} from '../Common/eventLogsMapStateToProps';

interface Props extends EventsLogsInjectedProps {}

const PendingEvents: React.FC<Props> = props => {
  const { dispatch, triggerName, readOnlyMode, currentSource } = props;

  const renderRows: FilterRenderProp = (
    rows,
    count,
    filterState,
    setFilterState,
    runQuery
  ) => (
    <EventsTable
      rows={rows}
      count={count}
      filterState={filterState}
      setFilterState={setFilterState}
      runQuery={runQuery}
      columns={['id', 'delivered', 'created_at', 'tries']}
      identifier={triggerName}
      triggerType="data"
    />
  );

  return (
    <React.Fragment>
      <TableHeader
        count={null}
        triggerName={triggerName}
        tabName="pending"
        readOnlyMode={readOnlyMode}
      />
      <br />
      <FilterQuery
        dispatch={dispatch}
        table={etEventsTable}
        render={renderRows}
        presets={{
          filters: [
            makeValueFilter('trigger_name', '$eq', triggerName),
            makeValueFilter('archived', '$eq', 'false'),
            makeValueFilter('delivered', '$eq', 'false'),
            makeValueFilter('error', '$eq', 'false'),
          ],
          sorts: [makeOrderBy('created_at', 'asc')],
        }}
        relationships={['logs']}
        triggerName={triggerName}
        triggerOp="pending"
        triggerType="data"
        currentSource={currentSource}
      />
    </React.Fragment>
  );
};

const PendingEventConnector = connector(PendingEvents);
export default PendingEventConnector;
