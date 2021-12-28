import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { mapDispatchToPropsEmpty } from '../../Common/utils/reactUtils';
import { ReduxState } from '../../../types';
import TableHeader from './TableCommon/TableHeader';
import { RightContainer } from '../../Common/Layout/RightContainer';
import RawSqlButton from './Common/Components/RawSqlButton';
import { driverToLabel } from '../../../dataSources';

type OwnProps = {
  tableName: string;
  schemaName: string;
  tab: string;
  showTab?: boolean;
  tableType?: string;
};

function FeatureDisabled({
  dispatch,
  tab,
  currentDataSource,
  readOnlyMode,
  migrationMode,
  schemaName,
  tableName,
  tableType,
}: Props) {
  return (
    <RightContainer>
      <TableHeader
        count={0}
        isCountEstimated
        dispatch={dispatch}
        table={{
          table_name: tableName,
          table_schema: schemaName,
          table_type: tableType,
        }}
        source={currentDataSource}
        tabName={tab}
        migrationMode={migrationMode}
        readOnlyMode={readOnlyMode}
      />

      <div
        style={{
          padding: '10px 12px',
          borderRadius: '4px',
          background: '#dae0e8',
          marginBottom: '20px',
          width: '50%',
          marginTop: '10px',
        }}
      >
        <p style={{ marginBottom: '5px' }}>
          <i className="fa fa-flask" aria-hidden="true" />{' '}
          <b>Coming soon for {driverToLabel.mssql}</b>
        </p>
        <p style={{ marginBottom: '5px' }}>
          This feature is currently unavailable for {driverToLabel.mssql}, but
          we are actively working on making it available for the Console.
        </p>
        <p style={{ marginBottom: '5px' }}>
          For all row and column based operations our SQL runner is available.
        </p>
        <RawSqlButton sql="" dispatch={dispatch}>
          Go to SQL Runner
        </RawSqlButton>
      </div>
    </RightContainer>
  );
}

const mapStateToProps = (state: ReduxState) => {
  return {
    currentDataSource: state.tables.currentDataSource,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    currentSchema: state.tables.currentSchema,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

interface Props extends ConnectedProps<typeof connector>, OwnProps {}

export default connector(FeatureDisabled);
