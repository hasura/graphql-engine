import React from 'react';
import { connect } from 'react-redux';

import RootFieldsEditor from './RootFieldsEditor';
import Tooltip from '../../../Common/Tooltip/Tooltip';

import {
  getTableCustomRootFields,
  getTableCustomName,
} from '../../../../dataSources';

import styles from './ModifyTable.scss';

const RootFields = props => {
  const { tableSchema, rootFieldsEdit, dispatch, customName } = props;

  const existingRootFields = getTableCustomRootFields(tableSchema);
  const existingCustomName = getTableCustomName(tableSchema);

  return (
    <React.Fragment>
      <h4 className={styles.subheading_text}>
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
      />
    </React.Fragment>
  );
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableSchema: ownProps.tableSchema,
    rootFieldsEdit: state.tables.modify.rootFieldsEdit,
    customName: state.tables.modify.custom_name,
  };
};

export default connect(mapStateToProps)(RootFields);
