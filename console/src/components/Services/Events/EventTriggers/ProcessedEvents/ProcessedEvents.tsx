import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import { FilterRenderProp } from '../../../../Common/FilterQuery/utils';
import { etEventsTable } from '../utils';
import { ReduxState } from '../../../../../Types';
import TableHeader from '../TableCommon/TableHeader';
import EventsTable from '../../Common/Components/EventsTable';

type ProcessedEventsProps = {
  dispatch: any;
  triggerName: string;
  readOnlyMode: boolean;
};

const ProcessedEvents = (props: ProcessedEventsProps) => {
  const { dispatch, triggerName, readOnlyMode } = props;

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
      triggerName={triggerName}
    />
  );

  return (
    <div>
      <TableHeader
        count={null}
        triggerName={triggerName}
        tabName="processed"
        readOnlyMode={readOnlyMode}
      />
      <br />
      <FilterQuery
        table={etEventsTable}
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

const mapStateToProps = (state: ReduxState, ownProps: any) => {
  return {
    triggerName: ownProps.params.triggerName,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const processedEventsConnector = (connect: any) =>
  connect(mapStateToProps)(ProcessedEvents);

export default processedEventsConnector;
