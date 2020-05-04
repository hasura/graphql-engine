import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import { FilterRenderProp } from '../../../../Common/FilterQuery/utils';
import { stEventsTable } from '../utils';
import { ScheduledTrigger } from '../../Types';
import EventsTable from '../../Common/Components/EventsTable';

type PendingEventsProps = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
  readOnlyMode: boolean;
};

const PendingEvents = (props: PendingEventsProps) => {
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
            { kind: 'value', key: 'archived', operator: '$eq', value: 'false' },
            {
              kind: 'value',
              key: 'delivered',
              operator: '$eq',
              value: 'false',
            },
            { kind: 'value', key: 'error', operator: '$eq', value: 'false' },
          ],
          sorts: [{ column: 'created_at', type: 'desc' }],
        }}
        relationships={['logs']}
      />
    </div>
  );
};

export default PendingEvents;
