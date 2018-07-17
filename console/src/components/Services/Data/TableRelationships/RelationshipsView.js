import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ViewHeader from '../TableBrowseRows/ViewHeader';
import { RESET } from '../TableModify/ModifyActions';
import {
  deleteRelMigrate,
  addNewRelClicked,
  relTableChange,
  relNameChanged,
  REL_SET_LCOL,
  REL_SET_RCOL,
  relTypeChange,
  addRelViewMigrate,
} from './Actions';
import { findAllFromRel } from '../utils';
import { setTable } from '../DataActions';

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
  const getGrayText = value => <i>{value}</i>;
  return isObjRel ? (
    <span>
      &nbsp;{getGrayText(lcol)}&nbsp;&nbsp;&rarr;&nbsp;&nbsp;{rTable} :: {rcol}
    </span>
  ) : (
    <span>
      &nbsp;{rTable.name} :: {rcol}&nbsp;&nbsp;&rarr;&nbsp;&nbsp;{getGrayText(
        lcol
      )}
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
        <button className="btn btn-sm btn-danger" onClick={onDelete}>
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

const AddRelationship = ({
  tableName,
  allSchemas,
  manualColumns,
  dispatch,
}) => {
  // eslint-disable-line no-unused-vars
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
      <div>
        <div className={styles.subheading_text}> Add New Relationship </div>
        <div className="form-group">
          <div className={`${styles.relBlockInline} ${styles.relBlockLeft}`}>
            Relationship Type
          </div>
          <div className={`${styles.relBlockInline} ${styles.relBlockRight}`}>
            <select
              data-test="data-rel-type"
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
          data-test="view-add-relationship"
        >
          Add Relationship
        </button>
      </div>
    </div>
  );
};

class RelationshipsView extends Component {
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
        <ViewHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="relationships"
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
                  dispatch(addNewRelClicked());
                }}
              >
                + Add relationship
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
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  allSchemas: state.tables.allSchemas,
  currentSchema: state.tables.currentSchema,
  ...state.tables.modify,
});

const relationshipsViewConnector = connect =>
  connect(mapStateToProps)(RelationshipsView);

export default relationshipsViewConnector;
