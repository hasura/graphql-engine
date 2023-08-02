import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
  makeOperationFilter,
} from '../../../../Common/FilterQuery/types';
import { etEventsTable } from '../utils';
import TableHeader from '../TableCommon/TableHeader';
import EventsTable from '../../Common/Components/EventsTable';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import {
  connector,
  EventsLogsInjectedProps,
} from '../Common/eventLogsMapStateToProps';

type Props = EventsLogsInjectedProps;

const ProcessedEvents: React.FC<Props> = props => {
  const { dispatch, triggerName, currentTrigger, readOnlyMode } = props;

  const renderRows: FilterRenderProp = (
    rows,

    filterState,
    setFilterState,
    runQuery
  ) => (
    <div className="bootstrap-jail">
      <EventsTable
        rows={rows}
        filterState={filterState}
        setFilterState={setFilterState}
        runQuery={runQuery}
        columns={['id', 'delivered', 'created_at', 'tries']}
        identifier={triggerName}
        triggerType="data"
      />
    </div>
  );

  return (
    <React.Fragment>
      <TableHeader
        count={null}
        triggerName={triggerName}
        tabName="processed"
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
            makeOperationFilter('$or', [
              makeValueFilter('delivered', '$eq', 'true'),
              makeValueFilter('error', '$eq', 'true'),
              makeValueFilter('archived', '$eq', 'true'),
            ]),
          ],
          sorts: [makeOrderBy('created_at', 'desc')],
        }}
        relationships={['logs']}
        triggerName={triggerName}
        triggerType="data"
        triggerOp="processed"
        currentSource={currentTrigger.source}
      />
    </React.Fragment>
  );
};

export const ProcessedEventsConnector = connector(ProcessedEvents);

export default ProcessedEventsConnector;
