import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
  makeOperationFilter,
} from '../../../../Common/FilterQuery/Types';
import { etEventsTable } from '../utils';
import { MapReduxToProps, ComponentReduxConnector } from '../../../../../Types';
import TableHeader from '../TableCommon/TableHeader';
import EventsTable from '../../Common/Components/EventsTable';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';

type Props = {
  dispatch: any;
  triggerName: string;
  readOnlyMode: boolean;
};

const ProcessedEvents: React.FC<Props> = props => {
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
            makeValueFilter('trigger_name', '$eq', triggerName),
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

const mapStateToProps: MapReduxToProps = (state, ownProps) => {
  return {
    triggerName: ownProps.params.triggerName,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector: ComponentReduxConnector = (connect: any) =>
  connect(mapStateToProps)(ProcessedEvents);

export default connector;
