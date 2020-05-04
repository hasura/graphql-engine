import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import { FilterRenderProp } from '../../../../Common/FilterQuery/utils';
import { etEventsTable } from '../utils';
import { ReduxState } from '../../../../../Types';
import TableHeader from '../TableCommon/TableHeader';
import EventsTable from '../../Common/Components/EventsTable';

type PendingEventsProps = {
  dispatch: any;
  triggerName: string;
  readOnlyMode: boolean;
};

const PendingEvents = (props: PendingEventsProps) => {
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
        tabName="pending"
        readOnlyMode={readOnlyMode}
      />
      <br />
      <FilterQuery
        table={etEventsTable}
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

const mapStateToProps = (state: ReduxState, ownProps: any) => {
  return {
    triggerName: ownProps.params.triggerName,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const pendingEventsConnector = (connect: any) =>
  connect(mapStateToProps)(PendingEvents);

export default pendingEventsConnector;
