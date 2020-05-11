import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
  makeOperationFilter,
} from '../../../../Common/FilterQuery/Types';
import { stEventsTable } from '../utils';
import { ScheduledTrigger } from '../../Types';
import EventsTable from '../../Common/Components/EventsTable';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';

type Props = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
};

const ProcessedEvents: React.FC<Props> = props => {
  const { dispatch, currentTrigger } = props;

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
      columns={['id', 'delivered', 'created_at']}
      triggerName={currentTrigger ? currentTrigger.name : ''}
    />
  );

  return (
    <div>
      <FilterQuery
        table={stEventsTable}
        dispatch={dispatch}
        render={renderRows}
        presets={{
          filters: [
            makeOperationFilter('$or', [
              makeValueFilter('delivered', '$eq', 'true'),
              makeValueFilter('error', '$eq', 'true'),
            ]),
          ],
          sorts: [makeOrderBy('created_at', 'desc')],
        }}
        relationships={['logs']}
      />
    </div>
  );
};

export default ProcessedEvents;
