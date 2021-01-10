import React from 'react';
import { connect } from 'react-redux';

import RootFieldsEditor from './RootFieldsEditor';
import Tooltip from '../../../Common/Tooltip/Tooltip';

import styles from './ModifyTable.scss';
import { getTableCustomRootFields } from '../../../../dataSources';
import { getTableName } from '../utils';

const RootFields = props => {
  const { tableSchema, rootFieldsEdit, dispatch } = props;

  const existingRootFields = getTableCustomRootFields(tableSchema);

  return (
    <React.Fragment>
      <h4 className={styles.subheading_text}>
        Custom GraphQL Root Fields
        <Tooltip message="Change the root fields for the table/view in the GraphQL API" />
      </h4>
      <RootFieldsEditor
        existingRootFields={existingRootFields}
        rootFieldsEdit={rootFieldsEdit}
        dispatch={dispatch}
        tableName={getTableName(tableSchema)}
      />
      <hr />
    </React.Fragment>
  );
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableSchema: ownProps.tableSchema,
    rootFieldsEdit: state.tables.modify.rootFieldsEdit,
  };
};

export default connect(mapStateToProps)(RootFields);
