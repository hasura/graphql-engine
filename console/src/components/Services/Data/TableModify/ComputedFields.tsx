import PropTypes from 'prop-types';
import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import Tooltip from '../../../Common/Tooltip/Tooltip';
import styles from './ModifyTable.scss';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';
import ComputedFieldsEditor from './ComputedFieldsEditor';
import { ReduxState } from '../../../../types';
import { Table } from '../../../../dataSources/types';

const ComputedFields = (props: ComputedFieldsProps) => {
  const {
    dispatch,
    currentSchema,
    tableSchema,
    nonTrackableFunctions,
    trackableFunctions,
    schemaList,
    currentSource,
  } = props;

  const allFunctions = nonTrackableFunctions.concat(trackableFunctions);

  return (
    <React.Fragment>
      <h4 className={styles.subheading_text}>
        Computed fields
        <Tooltip message="Add a function as a virtual field in the GraphQL API" />
        <KnowMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/computed-fields.html" />
      </h4>
      <ComputedFieldsEditor
        table={tableSchema}
        currentSchema={currentSchema}
        functions={allFunctions} // TODO: fix cross schema functions
        schemaList={schemaList}
        dispatch={dispatch}
        source={currentSource}
      />
    </React.Fragment>
  );
};

ComputedFields.propTypes = {
  currentSchema: PropTypes.string.isRequired,
  dispatch: PropTypes.func.isRequired,
};

type OwnProps = {
  tableSchema: Table;
};

const mapStateToProps = (state: ReduxState, ownProps: OwnProps) => {
  return {
    tableSchema: ownProps.tableSchema,
    currentSchema: state.tables.currentSchema,
    currentSource: state.tables.currentDataSource,
    nonTrackableFunctions: state.tables.nonTrackablePostgresFunctions || [],
    trackableFunctions: state.tables.postgresFunctions || [],
    schemaList: state.tables.schemaList,
  };
};

const connector = connect(mapStateToProps);
type InjectedProps = ConnectedProps<typeof connector>;
type ComputedFieldsProps = OwnProps & InjectedProps;

const ConnectedComputedFields = connector(ComputedFields);
export default ConnectedComputedFields;
