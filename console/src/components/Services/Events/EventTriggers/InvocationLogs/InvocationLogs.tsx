import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
  makeRelationshipFilter,
} from '../../../../Common/FilterQuery/Types';
import { etInvocationLogsTable } from '../utils';
import { MapReduxToProps, ComponentReduxConnector } from '../../../../../Types';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import TableHeader from '../TableCommon/TableHeader';
import InvocationLogsTable from '../../Common/Components/InvocationLogsTable';

type Props = {
  dispatch: any;
  triggerName: string;
  readOnlyMode: boolean;
};

const InvocationLogs: React.FC<Props> = props => {
  const { dispatch, triggerName, readOnlyMode } = props;

  const renderRows: FilterRenderProp = (
    rows,
    count,
    filterState,
    setFilterState,
    runQuery
  ) => (
    <InvocationLogsTable
      rows={rows}
      filterState={filterState}
      count={count}
      setFilterState={setFilterState}
      runQuery={runQuery}
      columns={[
        'id',
        'redeliver',
        'status',
        'event_id',
        // 'operation',
        'created_at',
      ]}
      identifier={triggerName}
      dispatch={dispatch}
    />
  );

  return (
    <div>
      <TableHeader
        count={null}
        triggerName={triggerName}
        tabName="logs"
        readOnlyMode={readOnlyMode}
      />
      <br />
      <FilterQuery
        table={etInvocationLogsTable}
        dispatch={dispatch}
        render={renderRows}
        relationships={['event']}
        presets={{
          sorts: [makeOrderBy('created_at', 'desc')],
          filters: [
            makeRelationshipFilter(
              'event',
              makeValueFilter('trigger_name', '$eq', triggerName)
            ),
          ],
        }}
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

const invocationLogsConnector: ComponentReduxConnector = connect =>
  connect(mapStateToProps)(InvocationLogs);

export default invocationLogsConnector;
