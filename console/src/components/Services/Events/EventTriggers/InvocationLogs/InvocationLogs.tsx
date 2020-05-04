import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import { FilterRenderProp } from '../../../../Common/FilterQuery/utils';
import { etInvocationLogsTable } from '../utils';
import { ReduxState } from '../../../../../Types';
import TableHeader from '../TableCommon/TableHeader';
import InvocationLogsTable from '../../Common/Components/InvocationLogsTable';

type InvocationLogsProps = {
  dispatch: any;
  triggerName: string;
  readOnlyMode: boolean;
};

const InvocationLogs = (props: InvocationLogsProps) => {
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
        // 'redeliver',
        'status',
        'event_id',
        // 'operation',
        'created_at',
      ]}
      triggerName={triggerName}
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
        presets={{
          sorts: [{ column: 'created_at', type: 'desc' }],
          filters: [
            {
              kind: 'relationship',
              key: 'event',
              value: {
                kind: 'value',
                key: 'trigger_name',
                operator: '$eq',
                value: triggerName,
              },
            },
          ],
        }}
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

const invocationLogsConnector = (connect: any) =>
  connect(mapStateToProps)(InvocationLogs);

export default invocationLogsConnector;
