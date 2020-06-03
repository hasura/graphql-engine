import React, { Component } from 'react';
import PropTypes from 'prop-types';
import TableHeader from '../TableCommon/TableHeader';
import { findAllFromRel } from '../utils';
import { getObjArrRelList } from './utils';
import { setTable, UPDATE_REMOTE_SCHEMA_MANUAL_REL } from '../DataActions';
import AddManualRelationship from './AddManualRelationship';
import RelationshipEditor from './RelationshipEditor';
import { NotFoundError } from '../../../Error/PageNotFound';
import RemoteRelationships from './RemoteRelationships/RemoteRelationships';
import { fetchRemoteSchemas } from '../../RemoteSchema/Actions';

class RelationshipsView extends Component {
  componentDidMount() {
    const { dispatch, currentSchema, tableName } = this.props;
    dispatch(setTable(tableName));
    // Sourcing the current schema into manual relationship
    dispatch({
      type: UPDATE_REMOTE_SCHEMA_MANUAL_REL,
      data: currentSchema,
    });
    dispatch(fetchRemoteSchemas());
  }

  render() {
    const {
      tableName,
      allSchemas,
      ongoingRequest,
      lastError,
      lastFormError,
      lastSuccess,
      dispatch,
      manualRelAdd,
      currentSchema,
      migrationMode,
      readOnlyMode,
      schemaList,
      remoteSchemas,
    } = this.props;

    const styles = require('../TableModify/ModifyTable.scss');
    const tableStyles = require('../../../Common/TableCommon/TableStyles.scss');

    const tableSchema = allSchemas.find(
      t => t.table_name === tableName && t.table_schema === currentSchema
    );

    if (!tableSchema) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    let alert = null;
    if (ongoingRequest) {
      alert = (
        <div className="hidden alert alert-warning" role="alert">
          Saving...
        </div>
      );
    } else if (lastError) {
      alert = (
        <div className="hidden alert alert-danger" role="alert">
          Error: {JSON.stringify(lastError)}
        </div>
      );
    } else if (lastSuccess) {
      alert = (
        <div className="hidden alert alert-success" role="alert">
          Saved!
        </div>
      );
    } else if (lastFormError) {
      alert = (
        <div className="hidden alert alert-warning" role="alert">
          {lastFormError}
        </div>
      );
    }

    const objArrRelList = getObjArrRelList(tableSchema.relationships);

    let addedRelationshipsView = null;
    if (objArrRelList.length > 0) {
      addedRelationshipsView = (
        <div className={tableStyles.tableContainer}>
          <table
            className={`${tableStyles.table} table table-bordered table-striped table-hover`}
          >
            <thead>
              <tr>
                {['Object relationships', 'Array relationships'].map((s, i) => (
                  <th key={i}>{s}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {objArrRelList.map(rel => {
                const column1 = rel.objRel ? (
                  <RelationshipEditor
                    dispatch={dispatch}
                    key={rel.objRel.rel_name}
                    relConfig={findAllFromRel(
                      allSchemas,
                      tableSchema,
                      rel.objRel
                    )}
                  />
                ) : (
                  <td />
                );
                const column2 = rel.arrRel ? (
                  <RelationshipEditor
                    key={rel.arrRel.rel_name}
                    dispatch={dispatch}
                    relConfig={findAllFromRel(
                      allSchemas,
                      tableSchema,
                      rel.arrRel
                    )}
                  />
                ) : (
                  <td />
                );
                return (
                  <tr>
                    {column1}
                    {column2}
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      );
    }

    const remoteRelationshipsSection = () => {
      return (
        <div className={`${styles.padd_left_remove} col-xs-10 col-md-10`}>
          <h4 className={styles.subheading_text}>Remote Relationships</h4>
          <RemoteRelationships
            relationships={tableSchema.remote_relationships}
            reduxDispatch={dispatch}
            table={tableSchema}
            remoteSchemas={remoteSchemas}
          />
        </div>
      );
    };

    return (
      <div className={`${styles.container} container-fluid`}>
        <TableHeader
          dispatch={dispatch}
          table={tableSchema}
          tabName="relationships"
          migrationMode={migrationMode}
          readOnlyMode={readOnlyMode}
        />
        <br />
        <div className={`${styles.padd_left_remove} container-fluid`}>
          <div className={`${styles.padd_left_remove} col-xs-10 col-md-10`}>
            <h4 className={styles.subheading_text}>Relationships</h4>
            {addedRelationshipsView}
            <br />
            <AddManualRelationship
              tableSchema={tableSchema}
              allSchemas={allSchemas}
              schemaList={schemaList}
              relAdd={manualRelAdd}
              dispatch={dispatch}
            />
            <hr />
          </div>
          {remoteRelationshipsSection()}
        </div>
        <div className={`${styles.fixed} hidden`}>{alert}</div>
      </div>
    );
  }
}

RelationshipsView.propTypes = {
  tableName: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  currentSchema: PropTypes.string.isRequired,
  activeEdit: PropTypes.object.isRequired,
  manualRelAdd: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  readOnlyMode: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastFormError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
  serverVersion: PropTypes.string,
  remoteSchemas: PropTypes.array.isRequired,
  featuresCompatibility: PropTypes.object,
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  allSchemas: state.tables.allSchemas,
  currentSchema: state.tables.currentSchema,
  migrationMode: state.main.migrationMode,
  readOnlyMode: state.main.readOnlyMode,
  serverVersion: state.main.serverVersion,
  schemaList: state.tables.schemaList,
  remoteSchemas: state.remoteSchemas.listData.remoteSchemas.map(r => r.name),
  ...state.tables.modify,
});

const relationshipsViewConnector = connect =>
  connect(mapStateToProps)(RelationshipsView);

export default relationshipsViewConnector;
