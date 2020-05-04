import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import { FilterRenderProp } from '../../../../Common/FilterQuery/utils';
import { stEventsTable } from '../utils';
import { ScheduledTrigger } from '../../Types';
import EventsTable from '../../Common/Components/EventsTable';

type ProcessedEventsProps = {
  dispatch: any;
  readOnlyMode: boolean;
  currentTrigger?: ScheduledTrigger;
};

const ProcessedEvents = (props: ProcessedEventsProps) => {
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
            {
              kind: 'operator',
              key: '$or',
              value: [
                {
                  kind: 'value',
                  key: 'delivered',
                  operator: '$eq',
                  value: 'true',
                },
                { kind: 'value', key: 'error', operator: '$eq', value: 'true' },
              ],
            },
          ],
          sorts: [{ column: 'created_at', type: 'desc' }],
        }}
        relationships={['logs']}
      />
    </div>
  );
};

export default ProcessedEvents;
