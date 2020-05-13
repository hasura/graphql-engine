import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
} from '../../../../Common/FilterQuery/Types';
import { etEventsTable } from '../utils';
import { MapReduxToProps, ComponentReduxConnector } from '../../../../../Types';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import TableHeader from '../TableCommon/TableHeader';
import EventsTable from '../../Common/Components/EventsTable';

type Props = {
  dispatch: any;
  triggerName: string;
  readOnlyMode: boolean;
};

const PendingEvents: React.FC<Props> = props => {
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
      columns={['id', 'status', 'created_at']}
      identifier={triggerName}
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
            makeValueFilter('trigger_name', '$eq', triggerName),
            makeValueFilter('archived', '$eq', 'false'),
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

const mapStateToProps: MapReduxToProps = (state, ownProps) => {
  return {
    triggerName: ownProps.params.triggerName,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const pendingEventsConnector: ComponentReduxConnector = connect =>
  connect(mapStateToProps)(PendingEvents);

export default pendingEventsConnector;
