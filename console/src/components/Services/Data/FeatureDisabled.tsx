import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { mapDispatchToPropsEmpty } from '../../Common/utils/reactUtils';
import { ReduxState } from '../../../types';
import TableHeader from './TableCommon/TableHeader';
import { RightContainer } from '../../Common/Layout/RightContainer';
import RawSqlButton from './Common/Components/RawSqlButton';

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
      <div style={{ height: '150px', paddingTop: '10px' }}>
        <p>
          <b>Coming soon: Data Management for MSSQL</b>
        </p>
        <p>
          We are currently working on bringing our full data management
          capabilities to MSSQL. <br /> For all row and column based operations
          our SQL runner is currently available.
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
