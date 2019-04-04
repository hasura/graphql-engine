import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ViewHeader from '../TableBrowseRows/ViewHeader';
import { RESET } from '../TableModify/ModifyActions';
import { addNewRelClicked } from './Actions';
import { findAllFromRel } from '../utils';
import { setTable, UPDATE_REMOTE_SCHEMA_MANUAL_REL } from '../DataActions';

import AddRelationship from './AddManualRelationship';
import Button from '../../../Common/Button/Button';
import RelationshipEditor from './RelationshipEditor';
import semverCheck from '../../../../helpers/semver';

/* Gets the complete list of relationships and converts it to a list of object, which looks like so :
{
objRel: {objectRelationship},
arrRel: {arrayRelationship}
} */
const getObjArrayRelationshipList = relationships => {
  const objRels = relationships.filter(r => r.rel_type === 'object');
  const arrRels = relationships.filter(r => r.rel_type !== 'object');
  const requiredList = [];
  const length =
    objRels.length > arrRels.length ? objRels.length : arrRels.length;
  for (let i = 0; i < length; i++) {
    const objRel = objRels[i] ? objRels[i] : null;
    const arrRel = arrRels[i] ? arrRels[i] : null;
    requiredList.push({
      objRel,
      arrRel,
    });
  }
  return requiredList;
};

class RelationshipsView extends Component {
  state = {
    supportRename: false,
  };
  componentDidMount() {
    const { dispatch, serverVersion, currentSchema, tableName } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(tableName));
    // Sourcing the current schema into manual relationship
    dispatch({
      type: UPDATE_REMOTE_SCHEMA_MANUAL_REL,
      data: currentSchema,
    });
    if (serverVersion) {
      this.checkRenameSupport(serverVersion);
    }
  }

  componentWillReceiveProps(nextProps) {
    if (
      nextProps.serverVersion &&
      nextProps.serverVersion !== this.props.serverVersion
    ) {
      this.checkRenameSupport(nextProps.serverVersion);
    }
  }

  checkRenameSupport = serverVersion => {
    try {
      if (semverCheck('tableColumnRename', serverVersion)) {
        this.setState({
          supportRename: true,
        });
      }
    } catch (e) {
      console.error(e);
    }
  };
  render() {
    const {
      tableName,
      allSchemas,
      ongoingRequest,
      lastError,
      lastFormError,
      lastSuccess,
      dispatch,
      relAdd,
      currentSchema,
      migrationMode,
      schemaList,
    } = this.props;
    const styles = require('../TableModify/ModifyTable.scss');
    const tableStyles = require('../../../Common/TableCommon/TableStyles.scss');

    const tableSchema = allSchemas.find(t => t.table_name === tableName);
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

    const addedRelationshipsView =
      getObjArrayRelationshipList(tableSchema.relationships).length > 0 ? (
        <div className={tableStyles.tableContainer}>
          <table
            className={`${
              tableStyles.table
            } table table-bordered table-striped table-hover`}
          >
            <thead>
              <tr>
                {['Object relationships', 'Array relationships'].map((s, i) => (
                  <th key={i}>{s}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {getObjArrayRelationshipList(tableSchema.relationships).map(
                rel => {
                  const column1 = rel.objRel ? (
                    <RelationshipEditor
                      dispatch={dispatch}
                      tableName={tableName}
                      relName={rel.objRel.rel_name}
                      key={rel.objRel.rel_name}
                      relConfig={findAllFromRel(
                        allSchemas,
                        tableSchema,
                        rel.objRel
                      )}
                      isObjRel
                      allowRename={this.state.supportRename}
                    />
                  ) : (
                    <td />
                  );
                  const column2 = rel.arrRel ? (
                    <RelationshipEditor
                      dispatch={dispatch}
                      tableName={tableName}
                      relName={rel.arrRel.rel_name}
                      key={rel.arrRel.rel_name}
                      relConfig={findAllFromRel(
                        allSchemas,
                        tableSchema,
                        rel.arrRel
                      )}
                      isObjRel={false}
                      allowRename={this.state.supportRename}
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
                }
              )}
            </tbody>
          </table>
        </div>
      ) : null;

    // if (tableSchema.primary_key.columns > 0) {}
    return (
      <div className={`${styles.container} container-fluid`}>
        <ViewHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="relationships"
          currentSchema={currentSchema}
          migrationMode={migrationMode}
        />
        <br />
        <div className={`${styles.padd_left_remove} container-fluid`}>
          <div className={`${styles.padd_left_remove} col-xs-10 col-md-10`}>
            <h4 className={styles.subheading_text}>Relationships</h4>
            {addedRelationshipsView}
            <br />
            {relAdd.isActive ? (
              <div className={styles.activeEdit}>
                <AddRelationship
                  tableName={tableName}
                  isObjRel={relAdd.isObjRel}
                  rTable={relAdd.rTable}
                  dispatch={dispatch}
                  lcol={relAdd.lcol}
                  rcol={relAdd.rcol}
                  allSchemas={allSchemas}
                  schemaList={schemaList}
                  manualRelInfo={relAdd.manualRelInfo}
                  manualColumns={relAdd.manualColumns}
                  titleInfo={'Add new relationship'}
                  currentSchema={currentSchema}
                  showClose={false}
                  dataTestVal={'view-add-relationship'}
                />
              </div>
            ) : (
              <Button
                type="submit"
                color="white"
                size="sm"
                onClick={() => {
                  dispatch(addNewRelClicked());
                }}
              >
                + Add relationship
              </Button>
            )}
            <hr />
          </div>
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
  fkAdd: PropTypes.object.isRequired,
  relAdd: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastFormError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
  serverVersion: PropTypes.string,
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  allSchemas: state.tables.allSchemas,
  currentSchema: state.tables.currentSchema,
  migrationMode: state.main.migrationMode,
  serverVersion: state.main.serverVersion,
  schemaList: state.tables.schemaList,
  ...state.tables.modify,
});

const relationshipsViewConnector = connect =>
  connect(mapStateToProps)(RelationshipsView);

export default relationshipsViewConnector;
