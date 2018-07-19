import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import { RESET } from '../TableModify/ModifyActions';
import {
  deleteRelMigrate,
  addNewRelClicked,
  addRelNewFromStateMigrate,
  relSelectionChanged,
  relNameChanged,
  resetRelationshipForm,
  relTableChange,
  REL_SET_LCOL,
  REL_SET_RCOL,
  relManualAddClicked,
  relTypeChange,
  addRelViewMigrate,
} from './Actions';
import { findAllFromRel } from '../utils';
import { showErrorNotification } from '../Notification';
import { setTable } from '../DataActions';
import gqlPattern, { gqlRelErrorNotif } from '../Common/GraphQLValidation';

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

/* This function sets the styling to the way the relationship looks, for eg: id -> user::user_id */
const getRelationshipLine = (isObjRel, lcol, rcol, rTable) => {
  const finalRTable = rTable.name ? rTable.name : rTable;
  return isObjRel ? (
    <span>
      &nbsp;{lcol}&nbsp;&nbsp;&rarr;&nbsp;&nbsp;{rTable} :: {rcol}
    </span>
  ) : (
    <span>
      &nbsp;{finalRTable} :: {rcol}&nbsp;&nbsp;&rarr;&nbsp;&nbsp;{lcol}
    </span>
  );
};

const relationshipView = (
  dispatch,
  tableName,
  relName,
  { lcol, rtable, rcol },
  isObjRel,
  tableStyles
) => {
  const onDelete = e => {
    e.preventDefault();
    const isOk = confirm('Are you sure?');
    if (isOk) {
      dispatch(deleteRelMigrate(tableName, relName));
    }
  };
  return (
    <td>
      <div>
        <button
          className="btn btn-sm btn-danger"
          onClick={onDelete}
          data-test={`remove-button-${relName}`}
        >
          Remove
        </button>
        &nbsp;
        <b>{relName}</b>
        <div className={tableStyles.relationshipTopPadding}>
          {getRelationshipLine(isObjRel, lcol, rcol, rtable)}
        </div>
      </div>
    </td>
  );
};

/* Returns all the possible relationships in the format
{
objRel: {},
arrRel: {}
} */
const suggestedRelationships = (tableName, allSchemas) => {
  const suggestedRelationshipArray = [];
  const objRels = [];
  const arrRels = [];
  const currentTableSchema = allSchemas.find(t => t.table_name === tableName);
  const currentTableRelationships = currentTableSchema.relationships;
  const currentObjRels = currentTableRelationships.filter(
    r => r.rel_type === 'object'
  );
  const currentArrRels = currentTableRelationships.filter(
    r => r.rel_type !== 'object'
  );
  for (let i = 0; i < allSchemas.length; i++) {
    const schema = allSchemas[i];
    const foreignKeyConstraints = schema.foreign_key_constraints;
    for (let j = 0; j < foreignKeyConstraints.length; j++) {
      const constraint = foreignKeyConstraints[j];
      if (constraint.table_name === tableName) {
        /* Object Relationships */
        const lcol = Object.keys(constraint.column_mapping)[0];
        let isExistingObjRel = false;
        for (let k = 0; k < currentObjRels.length; k++) {
          // check if this is already an existing relationship
          if (currentObjRels[k].rel_def.foreign_key_constraint_on === lcol) {
            // existing relationship
            isExistingObjRel = true;
          }
        }
        if (!isExistingObjRel) {
          objRels.push({
            tableName,
            isObjRel: true,
            name: null,
            lcol,
            rcol:
              constraint.column_mapping[
                Object.keys(constraint.column_mapping)[0]
              ],
            rTable: constraint.ref_table,
          });
        }
      } else if (constraint.ref_table === tableName) {
        /* Array Relationships */
        const rcol = Object.keys(constraint.column_mapping)[0];
        const rTable = constraint.table_name;
        let isExistingArrayRel = false;

        for (let k = 0; k < currentArrRels.length; k++) {
          // check if this is already an existing relationship
          const relDef = currentArrRels[k].rel_def;
          let currTable = null;
          let currRCol = null;

          if (relDef.foreign_key_constraint_on) {
            const tempTable = relDef.foreign_key_constraint_on.table;
            currTable = tempTable.name
              ? relDef.foreign_key_constraint_on.table.name
              : tempTable;
            currRCol = relDef.foreign_key_constraint_on.column;
          } else {
            currTable = relDef.manual_configuration.remote_table;
            currRCol = Object.values(
              relDef.manual_configuration.column_mapping
            )[0];
          }

          if (currRCol === rcol && currTable === constraint.table_name) {
            // existing relationship
            isExistingArrayRel = true;
          }
        }
        if (!isExistingArrayRel) {
          arrRels.push({
            tableName,
            isObjRel: false,
            name: null,
            rcol,
            lcol:
              constraint.column_mapping[
                Object.keys(constraint.column_mapping)[0]
              ],
            rTable,
          });
        }
      }

      /* Self Referencing Array Relationships */
      if (
        constraint.ref_table === tableName &&
        constraint.table_name === tableName
      ) {
        const rcol = Object.keys(constraint.column_mapping)[0];
        const rTable = constraint.table_name;
        let isExistingArrayRel = false;

        for (let k = 0; k < currentArrRels.length; k++) {
          // check if this is already an existing relationship
          if (
            currentArrRels[k].rel_def.foreign_key_constraint_on.column ===
              rcol &&
            currentArrRels[k].rel_def.foreign_key_constraint_on.table ===
              constraint.table_name
          ) {
            // existing relationship
            isExistingArrayRel = true;
          }
        }
        if (!isExistingArrayRel) {
          arrRels.push({
            tableName,
            isObjRel: false,
            name: null,
            rcol,
            lcol:
              constraint.column_mapping[
                Object.keys(constraint.column_mapping)[0]
              ],
            rTable,
          });
        }
      }
    }
  }

  const length =
    objRels.length > arrRels.length ? objRels.length : arrRels.length;
  for (let i = 0; i < length; i++) {
    const objRel = objRels[i] ? objRels[i] : null;
    const arrRel = arrRels[i] ? arrRels[i] : null;
    suggestedRelationshipArray.push({
      objRel,
      arrRel,
    });
  }

  return suggestedRelationshipArray;
};

const addRelationshipCellView = (
  dispatch,
  rel,
  selectedRelationship,
  selectedRelationshipName,
  tableStyles,
  relMetaData
) => {
  const onAdd = e => {
    e.preventDefault();
    dispatch(relSelectionChanged(rel));
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
  const styles = require('../TableModify/Modify.scss');

  return (
    <td>
      <div>
        {selectedRelationship === rel ? null : (
          <button
            className={`${styles.exploreButton} btn btn-xs`}
            onClick={onAdd}
            data-test={
              relMetaData[0] === 'object'
                ? `obj-rel-add-${relMetaData[1]}`
                : `arr-rel-add-${relMetaData[1]}`
            }
          >
            Add
          </button>
        )}
        {getRelationshipLine(rel.isObjRel, rel.lcol, rel.rcol, rel.rTable)}{' '}
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
            <button
              type="submit"
              className={`${styles.exploreButton} btn btn-sm`}
              data-test={
                relMetaData[0] === 'object'
                  ? `obj-rel-save-${relMetaData[1]}`
                  : `arr-rel-save-${relMetaData[1]}`
              }
            >
              Save
            </button>
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
  tableStyles,
}) => {
  // eslint-disable-line no-unused-vars
  const suggestedRelationshipsArray = suggestedRelationships(
    tableName,
    allSchemas
  );
  const styles = require('../TableModify/Modify.scss');
  if (suggestedRelationshipsArray.length < 1) {
    return (
      <div className={`${styles.remove_margin_bottom} form-group`}>
        <label>
          {' '}
          You have no new relationships that can be added. Add a foreign key{' '}
        </label>
      </div>
    );
  }
  // Finding the object from the suggestedRelationshipsArray which is currently selected
  let selectedRelationship = suggestedRelationshipsArray.find(rel => {
    let r = rel.arrRel;
    if (cachedRelationshipData.isObjRel) {
      r = rel.objRel;
    }
    if (!r) {
      return false;
    }
    return (
      r.lcol === cachedRelationshipData.lcol &&
      r.rcol === cachedRelationshipData.rcol &&
      r.rTable === cachedRelationshipData.rTable &&
      r.tableName === cachedRelationshipData.tableName
    );
  });

  /* selectedRelationship right now equals :
    {
      objRel: {SomeValue},
      arrRel: {SomeValue}
    }
  This strips it down to either objRel or arrRel */
  if (selectedRelationship) {
    selectedRelationship = cachedRelationshipData.isObjRel
      ? selectedRelationship.objRel
      : selectedRelationship.arrRel;
  }

  const relName = cachedRelationshipData.name
    ? cachedRelationshipData.name
    : '';

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
              ].map((s, i) => <th key={i}>{s}</th>)}
            </tr>
          </thead>
          <tbody>
            {suggestedRelationshipsArray.map((rel, i) => {
              const column1 = rel.objRel ? (
                addRelationshipCellView(
                  dispatch,
                  rel.objRel,
                  selectedRelationship,
                  relName,
                  tableStyles,
                  ['object', i]
                )
              ) : (
                <td />
              );
              const column2 = rel.arrRel ? (
                addRelationshipCellView(
                  dispatch,
                  rel.arrRel,
                  selectedRelationship,
                  relName,
                  tableStyles,
                  ['array', i]
                )
              ) : (
                <td />
              );
              return (
                <tr key={i}>
                  {column1}
                  {column2}
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
      <button
        className="btn btn-sm btn-default hide"
        onClick={e => {
          e.preventDefault();
          dispatch(resetRelationshipForm());
        }}
      >
        {' '}
        Cancel{' '}
      </button>
    </div>
  );
};

const AddManualRelationship = ({
  tableName,
  allSchemas,
  manualColumns,
  dispatch,
}) => {
  const styles = require('../TableModify/Modify.scss');
  const tableSchema = allSchemas.find(t => t.table_name === tableName);
  const onTableChange = e => {
    dispatch(relTableChange(e.target.value));
  };
  const onRelNameChange = e => {
    dispatch(relNameChanged(e.target.value));
  };
  const onRelLColChange = e => {
    dispatch({ type: REL_SET_LCOL, lcol: e.target.value });
  };
  const onRelRColChange = e => {
    dispatch({ type: REL_SET_RCOL, rcol: e.target.value });
  };
  const onRelTypeChange = e => {
    if (e.target.value === 'object_rel') {
      dispatch(relTypeChange('true'));
    } else {
      dispatch(relTypeChange('false'));
    }
  };
  const onAddRelClicked = () => {
    dispatch(addRelViewMigrate(tableName));
  };
  return (
    <div>
      <div className={styles.subheading_text}> Add a Manual Relationship </div>
      <div className="form-group">
        <div className={`${styles.relBlockInline} ${styles.relBlockLeft}`}>
          Relationship Type
        </div>
        <div className={`${styles.relBlockInline} ${styles.relBlockRight}`}>
          <select
            className="form-control"
            onChange={onRelTypeChange}
            data-test="rel-type"
          >
            <option key="select_type" value="select_type">
              Select relationship type
            </option>
            <option key="object" value="object_rel">
              Object Relationship
            </option>
            <option key="array" value="array_rel">
              Array Relationship
            </option>
          </select>
        </div>
      </div>
      <div className="form-group">
        <div className={`${styles.relBlockInline} ${styles.relBlockLeft}`}>
          Relationship Name
        </div>
        <div className={`${styles.relBlockInline} ${styles.relBlockRight}`}>
          <input
            onChange={onRelNameChange}
            className="form-control"
            placeholder="Enter relationship name"
            data-test="rel-name"
          />
        </div>
      </div>
      <div className="form-group">
        <div className={`${styles.relBlockInline} ${styles.relBlockLeft}`}>
          Configuration
        </div>
        <select
          className={`${styles.relBlockInline} form-control`}
          onChange={onRelLColChange}
          data-test="current-col"
        >
          <option key="default_column">Current Column</option>
          {tableSchema.columns.map((c, i) => (
            <option key={c + i} value={c.column_name}>
              {c.column_name}
            </option>
          ))}
        </select>
        <span> :: </span>
        <div className={styles.relBlockInline}>
          <select
            className="form-control"
            onChange={onTableChange}
            data-test="remote-table"
          >
            <option key="default_table">Remote Table</option>
            {allSchemas.map((s, i) => (
              <option key={i} value={s.table_name}>
                {s.table_name}
              </option>
            ))}
          </select>
        </div>
        <span> -> </span>
        <div className={styles.relBlockInline}>
          <select
            className="form-control"
            onChange={onRelRColChange}
            data-test="remote-table-col"
          >
            <option key="default_table_column">Remote Table Column:</option>
            {manualColumns.map((c, i) => (
              <option key={c + i} value={c.column_name}>
                {c.column_name}
              </option>
            ))}
          </select>
        </div>
      </div>
      <button
        className={styles.yellow_button}
        onClick={onAddRelClicked}
        data-test="table-add-manual-relationship"
      >
        Add
      </button>
    </div>
  );
};

class Relationships extends Component {
  componentDidMount() {
    this.props.dispatch({ type: RESET });

    this.props.dispatch(setTable(this.props.tableName));
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
      migrationMode,
      currentSchema,
    } = this.props;
    const styles = require('../TableModify/Modify.scss');
    const tableStyles = require('../TableCommon/TableStyles.scss');

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
                (rel, i) => {
                  const column1 = rel.objRel ? (
                    relationshipView(
                      dispatch,
                      tableName,
                      rel.objRel.rel_name,
                      findAllFromRel(allSchemas, tableSchema, rel.objRel),
                      true,
                      tableStyles
                    )
                  ) : (
                    <td />
                  );
                  const column2 = rel.arrRel ? (
                    relationshipView(
                      dispatch,
                      tableName,
                      rel.arrRel.rel_name,
                      findAllFromRel(allSchemas, tableSchema, rel.arrRel),
                      false,
                      tableStyles
                    )
                  ) : (
                    <td />
                  );
                  return (
                    <tr key={i}>
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
                  tableStyles={tableStyles}
                  dispatch={dispatch}
                />
              </div>
            ) : (
              <button
                type="submit"
                className="btn btn-sm btn-default"
                onClick={() => {
                  dispatch(addNewRelClicked());
                }}
              >
                + Add relationship
              </button>
            )}
            <hr />
          </div>
        </div>
        <div className={`${styles.padd_left_remove} container-fluid`}>
          <div className={`${styles.padd_left_remove} col-xs-8`}>
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
                  manualColumns={relAdd.manualColumns}
                />
              </div>
            ) : (
              <button
                type="submit"
                className="btn btn-sm btn-default"
                onClick={() => {
                  dispatch(relManualAddClicked());
                }}
                data-test="add-manual-relationship"
              >
                + Add a manual relationship
              </button>
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
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  allSchemas: state.tables.allSchemas,
  migrationMode: state.main.migrationMode,
  currentSchema: state.tables.currentSchema,
  ...state.tables.modify,
});

const relationshipsConnector = connect =>
  connect(mapStateToProps)(Relationships);

export default relationshipsConnector;

export { suggestedRelationships, getRelationshipLine };
