import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
} from '../../../../Common/FilterQuery/Types';
import { stEventsTable } from '../utils';
import { ScheduledTrigger } from '../../Types';
import EventsTable from '../../Common/Components/EventsTable';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';

type Props = {
  currentTrigger?: ScheduledTrigger;
  dispatch: any;
};

const PendingEvents: React.FC<Props> = props => {
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
      columns={['id', 'scheduled_at', 'created_at']}
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
            makeValueFilter('scheduled_time', '$gt', 'now()'),
            makeValueFilter('delivered', '$eq', 'false'),
            makeValueFilter('error', '$eq', 'false'),
          ],
          sorts: [makeOrderBy('created_at', 'desc')],
        }}
        relationships={['logs']}
      />
    </div>
  );
};

export default PendingEvents;
