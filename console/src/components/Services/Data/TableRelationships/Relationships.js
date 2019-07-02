import React, { Component } from 'react';
import PropTypes from 'prop-types';
import TableHeader from '../TableCommon/TableHeader';
import { RESET } from '../TableModify/ModifyActions';
import {
  addNewRelClicked,
  addRelNewFromStateMigrate,
  relSelectionChanged,
  relNameChanged,
  resetRelationshipForm,
  formRelName,
  getExistingFieldsMap,
} from './Actions';
import { findAllFromRel } from '../utils';
import { showErrorNotification } from '../../Common/Notification';
import { setTable } from '../DataActions';
import gqlPattern, { gqlRelErrorNotif } from '../Common/GraphQLValidation';
import { getRelDef, getObjArrRelList } from './utils';

import Button from '../../../Common/Button/Button';
import AddManualRelationship from './AddManualRelationship';
import suggestedRelationshipsRaw from './autoRelations';
import RelationshipEditor from './RelationshipEditor';
import { NotFoundError } from '../../../Error/PageNotFound';

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
          { custom: 'Relationship name cannot be empty' }
        )
      );
      return false;
    } else if (!gqlPattern.test(selectedRelationshipName)) {
      dispatch(
        showErrorNotification(
          gqlRelErrorNotif[0],
          gqlRelErrorNotif[1],
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
        {getRelDef(rel)} &nbsp;
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
  currentSchema,
  allSchemas,
  cachedRelationshipData,
  dispatch,
}) => {
  const styles = require('../TableModify/ModifyTable.scss');
  const tableStyles = require('../../../Common/TableCommon/TableStyles.scss');

  const cTable = allSchemas.find(
    t => t.table_name === tableName && t.table_schema === currentSchema
  );

  const suggestedRelationshipsData = suggestedRelationshipsRaw(
    tableName,
    allSchemas,
    currentSchema
  );

  if (
    suggestedRelationshipsData.objectRel.length < 1 &&
    suggestedRelationshipsData.arrayRel.length < 1
  ) {
    return (
      <div className={`${styles.remove_margin_bottom} form-group`}>
        <label>
          You have no new relationships that can be added via foreign-keys
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

  const relName = cachedRelationshipData.relName
    ? cachedRelationshipData.relName
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
        <label> Add new relationships via foreign-keys </label>
      </div>
      <div className={tableStyles.tableContainer}>
        <table
          className={`${
            tableStyles.table
          } table table-bordered table-striped table-hover`}
        >
          <thead>
            <tr>
              {[
                'Suggested Object Relationships',
                'Suggested Array Relationships',
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
  componentDidMount() {
    const { dispatch, tableName } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(tableName));
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
      relAdd,
      manualRelAdd,
      currentSchema,
      migrationMode,
      schemaList,
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

    return (
      <div className={`${styles.container} container-fluid`}>
        <TableHeader
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
                  currentSchema={currentSchema}
                  allSchemas={allSchemas}
                  cachedRelationshipData={relAdd}
                  dispatch={dispatch}
                />
                <hr />
                <AddManualRelationship
                  tableSchema={tableSchema}
                  allSchemas={allSchemas}
                  schemaList={schemaList}
                  relAdd={manualRelAdd}
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
  manualRelAdd: PropTypes.object.isRequired,
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
  currentSchema: state.tables.currentSchema,
  migrationMode: state.main.migrationMode,
  serverVersion: state.main.serverVersion,
  schemaList: state.tables.schemaList,
  ...state.tables.modify,
});

const relationshipsConnector = connect =>
  connect(mapStateToProps)(Relationships);

export default relationshipsConnector;
