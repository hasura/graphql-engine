import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import RootFieldsEditor from './RootFieldsEditor';
import Tooltip from '../../../Common/Tooltip/Tooltip';

import {
  getTableCustomRootFields,
  getTableCustomName,
  dataSource,
} from '../../../../dataSources';

import { ReduxState } from '../../../../types';
import { Table } from '../../../../dataSources/types';

const RootFields: React.FC<ComputedRootFieldsProps> = ({
  tableSchema,
  rootFieldsEdit,
  dispatch,
  customName,
  currentSchema,
}) => {
  const defaultSchemaForCurrentDatasource = dataSource.defaultRedirectSchema;

  const existingRootFields = getTableCustomRootFields(tableSchema);
  const existingCustomName = getTableCustomName(tableSchema);

  return (
    <React.Fragment>
      <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
        Custom GraphQL Root Fields
        <Tooltip message="Change the root fields for the table/view in the GraphQL API" />
      </h4>
      <RootFieldsEditor
        customName={customName}
        existingCustomName={existingCustomName}
        existingRootFields={existingRootFields}
        rootFieldsEdit={rootFieldsEdit}
        dispatch={dispatch}
        tableName={tableSchema.table_name}
        tableSchema={
          defaultSchemaForCurrentDatasource === currentSchema
            ? ''
            : currentSchema
        }
      />
    </React.Fragment>
  );
};

interface OwnProps {
  tableSchema: Table;
}

const mapStateToProps = (state: ReduxState, ownProps: OwnProps) => {
  return {
    tableSchema: ownProps.tableSchema,
    rootFieldsEdit: state.tables.modify.rootFieldsEdit,
    customName: state.tables.modify.custom_name,
    currentSchema: state.tables.currentSchema,
  };
};

const connector = connect(mapStateToProps);
type InjectedProps = ConnectedProps<typeof connector>;
type ComputedRootFieldsProps = OwnProps & InjectedProps;

const ConnectedRootFields = connector(RootFields);
export default ConnectedRootFields;
