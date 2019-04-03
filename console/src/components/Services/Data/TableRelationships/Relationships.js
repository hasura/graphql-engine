import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import { RESET } from '../TableModify/ModifyActions';
import {
  addNewRelClicked,
  addRelNewFromStateMigrate,
  relSelectionChanged,
  relNameChanged,
  resetRelationshipForm,
  relManualAddClicked,
  formRelName,
  getExistingFieldsMap,
} from './Actions';
import { findAllFromRel } from '../utils';
import { showErrorNotification } from '../Notification';
import { setTable } from '../DataActions';
import gqlPattern, { gqlRelErrorNotif } from '../Common/GraphQLValidation';
import { getRelDef } from './utils';

import AddManualRelationship from './AddManualRelationship';
import suggestedRelationshipsRaw from './autoRelations';
import RelationshipEditor from './RelationshipEditor';
import Button from '../../../Common/Button/Button';
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

const addRelationshipCellView = (
  dispatch,
  rel,
  selectedRelationship,
  selectedRelationshipName,
  relMetaData,
  tableSchema
) => {
  const tableStyles = require('../../../Common/TableCommon/TableStyles.scss');

  const onAdd = e => {
    e.preventDefault();
    dispatch(relSelectionChanged(rel));
    dispatch(
      relNameChanged(formRelName(rel, getExistingFieldsMap(tableSchema)))
    );
  };

  const onRelationshipNameChanged = e => {
    dispatch(relNameChanged(e.target.value));
  };

  const onSave = e => {
    e.preventDefault();
    if (!selectedRelationshipName.trim()) {
      dispatch(
        showErrorNotification(
          'Error adding relationship!',
          'Please select a name for the relationship',
          '',
          { custom: 'Relationship name cannot be empty' }
        )
      );
      return false;
    } else if (!gqlPattern.test(selectedRelationshipName)) {
      dispatch(
        showErrorNotification(
          gqlRelErrorNotif[0],
          gqlRelErrorNotif[1],
          gqlRelErrorNotif[2],
          gqlRelErrorNotif[3]
        )
      );
      return false;
    }
    dispatch(addRelNewFromStateMigrate());
  };
  return (
    <td>
      <div>
        {selectedRelationship === rel ? null : (
          <Button
            size="xs"
            color="yellow"
            onClick={onAdd}
            data-test={
              relMetaData[0] === 'object'
                ? `obj-rel-add-${relMetaData[1]}`
                : `arr-rel-add-${relMetaData[1]}`
            }
          >
            Add
          </Button>
        )}
        &nbsp;
        {getRelDef(
          rel.isObjRel,
          rel.lcol,
          rel.rcol,
          tableSchema.table_name,
          rel.rTable
        )}{' '}
        &nbsp;
      </div>
      {selectedRelationship === rel ? (
        <form className="form-inline" onSubmit={onSave}>
          <div className={`${tableStyles.relationshipTopPadding} form-group`}>
            <label> Name: </label> &nbsp;
            <input
              type="text"
              className="input-sm form-control"
              value={selectedRelationshipName}
              onChange={onRelationshipNameChanged}
              data-test="suggested-rel-name"
            />{' '}
            &nbsp;
            <Button
              type="submit"
              color="yellow"
              size="xs"
              data-test={
                relMetaData[0] === 'object'
                  ? `obj-rel-save-${relMetaData[1]}`
                  : `arr-rel-save-${relMetaData[1]}`
              }
            >
              Save
            </Button>
          </div>
        </form>
      ) : null}
    </td>
  );
};

const AddRelationship = ({
  tableName,
  allSchemas,
  cachedRelationshipData,
  dispatch,
}) => {
  const styles = require('../TableModify/ModifyTable.scss');
  const tableStyles = require('../../../Common/TableCommon/TableStyles.scss');

  const cTable = allSchemas.find(t => t.table_name === tableName);

  const suggestedRelationshipsData = suggestedRelationshipsRaw(
    tableName,
    allSchemas
  );

  if (
    suggestedRelationshipsData.objectRel.length < 1 &&
    suggestedRelationshipsData.arrayRel.length < 1
  ) {
    return (
      <div className={`${styles.remove_margin_bottom} form-group`}>
        <label>
          {' '}
          You have no new relationships that can be added. Add a foreign key to
          get suggestions{' '}
        </label>
      </div>
    );
  }
  let selectedRelationship;
  // Finding the object from the suggestedRelationshipsArray which is currently selected
  if (cachedRelationshipData.isObjRel) {
    selectedRelationship = suggestedRelationshipsData.objectRel.find(rel => {
      const cLcol =
        typeof cachedRelationshipData.lcol === 'string'
          ? [cachedRelationshipData.lcol]
          : cachedRelationshipData.lcol;
      const cRcol =
        typeof cachedRelationshipData.rcol === 'string'
          ? [cachedRelationshipData.rcol]
          : cachedRelationshipData.rcol;
      return (
        rel.lcol.join(',') === cLcol.join(',') &&
        rel.rcol.join(',') === cRcol.join(',') &&
        rel.rTable === cachedRelationshipData.rTable &&
        rel.tableName === cachedRelationshipData.tableName
      );
    });
  } else {
    selectedRelationship = suggestedRelationshipsData.arrayRel.find(rel => {
      const cLcol =
        typeof cachedRelationshipData.lcol === 'string'
          ? [cachedRelationshipData.lcol]
          : cachedRelationshipData.lcol;
      const cRcol =
        typeof cachedRelationshipData.rcol === 'string'
          ? [cachedRelationshipData.rcol]
          : cachedRelationshipData.rcol;
      return (
        rel.lcol.join(',') === cLcol.join(',') &&
        rel.rcol.join(',') === cRcol.join(',') &&
        rel.rTable === cachedRelationshipData.rTable &&
        rel.tableName === cachedRelationshipData.tableName
      );
    });
  }

  /* selectedRelationship right now equals :
    {
      objRel: {SomeValue},
      arrRel: {SomeValue}
    }
  This strips it down to either objRel or arrRel */

  const relName = cachedRelationshipData.name
    ? cachedRelationshipData.name
    : '';

  const column1 = [];
  const column2 = [];

  suggestedRelationshipsData.objectRel.map((rel, i) => {
    column1.push(
      rel.isObjRel ? (
        addRelationshipCellView(
          dispatch,
          rel,
          selectedRelationship,
          relName,
          ['object', i],
          cTable
        )
      ) : (
        <td />
      )
    );
  });

  suggestedRelationshipsData.arrayRel.map((rel, i) => {
    column2.push(
      rel.isObjRel ? (
        <td />
      ) : (
        addRelationshipCellView(
          dispatch,
          rel,
          selectedRelationship,
          relName,
          ['array', i],
          cTable
        )
      )
    );
  });

  const length =
    column1.length > column2.length ? column1.length : column2.length;

  const combinedRels = [];
  for (let i = 0; i < length; i++) {
    const objRel = column1[i] ? column1[i] : <td />;
    const arrRel = column2[i] ? column2[i] : <td />;
    combinedRels.push({
      objRel,
      arrRel,
    });
  }

  return (
    <div>
      <div>
        <label> Add a new relationship </label>
      </div>
      <div className={tableStyles.tableContainer}>
        <table
          className={`${
            tableStyles.relationshipTable
          } table table-bordered table-striped table-hover`}
        >
          <thead>
            <tr>
              {[
                'Suggested object relationships',
                'Suggested Array relationships',
              ].map((s, i) => (
                <th key={i}>{s}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {combinedRels.map((rel, i) => {
              return (
                <tr key={i}>
                  {rel.objRel}
                  {rel.arrRel}
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
      <Button
        className="hide"
        color="white"
        size="sm"
        onClick={e => {
          e.preventDefault();
          dispatch(resetRelationshipForm());
        }}
      >
        {' '}
        Cancel{' '}
      </Button>
    </div>
  );
};

class Relationships extends Component {
  state = {
    supportRename: false,
  };

  componentDidMount() {
    const { dispatch, serverVersion } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(this.props.tableName));
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
      migrationMode,
      currentSchema,
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
                      key={rel.objRel.rel_name}
                      relName={rel.objRel.rel_name}
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
                      key={rel.arrRel.rel_name}
                      dispatch={dispatch}
                      tableName={tableName}
                      relName={rel.arrRel.rel_name}
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
        <TableHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="relationships"
          migrationMode={migrationMode}
          currentSchema={currentSchema}
        />
        <br />
        <div className={`${styles.padd_left_remove} container-fluid`}>
          <div className={`${styles.padd_left_remove} col-xs-8`}>
            <h4 className={styles.subheading_text}>Relationships</h4>
            {addedRelationshipsView}
            <br />
            {relAdd.isActive ? (
              <div className={styles.activeEdit}>
                <AddRelationship
                  tableName={tableName}
                  allSchemas={allSchemas}
                  cachedRelationshipData={relAdd}
                  dispatch={dispatch}
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
        <div className={`${styles.padd_left_remove} container-fluid`}>
          <div className={`${styles.padd_left_remove} col-xs-10 col-md-10`}>
            {relAdd.isManualExpanded ? (
              <div className={styles.activeEdit}>
                <AddManualRelationship
                  tableName={tableName}
                  isObjRel={relAdd.isObjRel}
                  rTable={relAdd.rTable}
                  dispatch={dispatch}
                  lcol={relAdd.lcol}
                  rcol={relAdd.rcol}
                  allSchemas={allSchemas}
                  schemaList={schemaList}
                  manualColumns={relAdd.manualColumns}
                  manualRelInfo={relAdd.manualRelInfo}
                  titleInfo={'Add a relationship manually'}
                  currentSchema={currentSchema}
                  showClose
                  dataTestVal={'table-add-manual-relationship'}
                />
              </div>
            ) : (
              <Button
                type="submit"
                color="white"
                size="sm"
                onClick={() => {
                  dispatch(relManualAddClicked());
                }}
                data-test="add-manual-relationship"
              >
                + Add a relationship manually
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

Relationships.propTypes = {
  tableName: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  currentSchema: PropTypes.string.isRequired,
  activeEdit: PropTypes.object.isRequired,
  fkAdd: PropTypes.object.isRequired,
  relAdd: PropTypes.object.isRequired,
  migrationMode: PropTypes.bool.isRequired,
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
  migrationMode: state.main.migrationMode,
  serverVersion: state.main.serverVersion,
  currentSchema: state.tables.currentSchema,
  schemaList: state.tables.schemaList,
  ...state.tables.modify,
});

const relationshipsConnector = connect =>
  connect(mapStateToProps)(Relationships);

export default relationshipsConnector;

export { getRelDef };
