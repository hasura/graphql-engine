import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';

import {
  setTriggerName,
  setTableName,
  setSchemaName,
  setWebhookURL,
} from './AddActions';
import { setDefaults, createTableSql } from './AddActions';
import { resetValidation, fetchTableListBySchema } from './AddActions';

class AddTrigger extends Component {
  constructor(props) {
    super(props);
    this.props.dispatch(setDefaults());
    this.props.dispatch(fetchTableListBySchema('public'));
  }

  componentWillUnmount() {
    this.props.dispatch(setDefaults());
  }
  submitValidation() {
    this.props.dispatch(resetValidation());
    // table name validation
    if (this.tableNameCheck()) {
      // column validation.
      if (this.columnValidation()) {
        // The primary key validation ensure.
        this.props.dispatch(createTableSql());
      }
    }
  }
  render() {
    const {
      tableName,
      tableListBySchema,
      schemaName,
      schemaList,
      dispatch,
      ongoingRequest,
      lastError,
      lastSuccess,
      internalError,
    } = this.props;
    const styles = require('../TableCommon/Table.scss');
    let createBtnText = 'Create';
    if (ongoingRequest) {
      createBtnText = 'Creating...';
    } else if (lastError) {
      createBtnText = 'Creating Failed. Try again';
    } else if (internalError) {
      createBtnText = 'Creating Failed. Try again';
    } else if (lastSuccess) {
      createBtnText = 'Created! Redirecting...';
    }
    const updateTableList = e => {
      dispatch(setSchemaName(e.target.value));
      dispatch(fetchTableListBySchema(e.target.value));
    };
    const updateTableSelection = e => {
      dispatch(setTableName(e.target.value));
    };

    const getColumnList = operation => {
      const dispatchToggleColumn = e => {
        const column = e.target.value;
        // dispatch(permToggleColumn(column));
      };
      const tableSchema = tableListBySchema.find(
        t => t.table_name === tableName
      );
      console.log(tableSchema);

      if (tableSchema) {
        return tableSchema.columns.map((colObj, i) => {
          const column = colObj.column_name;
          /*
          const checked = permsState[query]
            ? permsState[query].columns.indexOf(column) !== -1
            : false;
          */
          const checked = false;

          return (
            <div
              key={i}
              className={styles.display_inline + ' ' + styles.add_mar_right}
            >
              <div className="checkbox">
                <label>
                  <input
                    type="checkbox"
                    checked={checked}
                    value={column}
                    onChange={dispatchToggleColumn}
                  />
                  {column}
                </label>
              </div>
            </div>
          );
        });
      }
      return null;
    };

    return (
      <div
        className={`${styles.addTablesBody} ${styles.main_wrapper} ${
          styles.padd_left
        }`}
      >
        <Helmet title="Add Trigger - Events | Hasura" />
        <div className={styles.subHeader}>
          <h2 className={styles.heading_text}>Add a new trigger</h2>
          <div className="clearfix" />
        </div>
        <br />
        <div className={`container-fluid ${styles.padd_left_remove}`}>
          <div
            className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove}`}
          >
            <h4 className={styles.subheading_text}>
              Trigger Name &nbsp; &nbsp;
            </h4>
            <input
              type="text"
              data-test="tableName"
              placeholder="trigger_name"
              className={`${styles.tableNameInput} form-control`}
              onChange={e => {
                dispatch(setTriggerName(e.target.value));
              }}
            />
            <hr />
            <h4 className={styles.subheading_text}>
              Schema/Table &nbsp; &nbsp;
            </h4>
            <select
              onChange={updateTableList}
              className={styles.selectTrigger + ' form-control'}
            >
              {schemaList.map(s => {
                if (s.schema_name === schemaName) {
                  return (
                    <option
                      value={s.schema_name}
                      key={s.schema_name}
                      selected="selected"
                    >
                      {s.schema_name}
                    </option>
                  );
                }
                return (
                  <option value={s.schema_name} key={s.schema_name}>
                    {s.schema_name}
                  </option>
                );
              })}
            </select>
            <select
              onChange={updateTableSelection}
              className={
                styles.selectTrigger + ' form-control ' + styles.add_mar_left
              }
            >
              <option>Select table</option>
              {tableListBySchema.map(t => {
                return <option key={t.table_name}>{t.table_name}</option>;
              })}
            </select>
            <hr />
            {tableName ? (
              <div>
                <h4 className={styles.subheading_text}>
                  Advanced - Operation/Columns &nbsp; &nbsp;
                </h4>
                <div>
                  <div>
                    <label>Insert</label>
                  </div>
                  {getColumnList('insert')}
                </div>
                <div>
                  <div>
                    <label>Update</label>
                  </div>
                  {getColumnList('update')}
                </div>
                <label>Delete</label>
                <hr />
              </div>
            ) : null}
            <h4 className={styles.subheading_text}>
              Webhook URL &nbsp; &nbsp;
            </h4>
            <input
              type="text"
              data-test="webhook"
              placeholder="webhook url"
              className={`${styles.tableNameInput} form-control`}
              onChange={e => {
                dispatch(setWebhookURL(e.target.value));
              }}
            />
            <hr />
            <button
              type="submit"
              className={`btn ${styles.yellow_button}`}
              onClick={this.submitValidation.bind(this)}
              data-test="table-create"
            >
              {createBtnText}
            </button>
          </div>
        </div>
      </div>
    );
  }
}

AddTrigger.propTypes = {
  triggerName: PropTypes.string,
  tableName: PropTypes.string,
  schemaName: PropTypes.string,
  schemaList: PropTypes.array,
  tableListBySchema: PropTypes.array,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  internalError: PropTypes.string,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state.addTrigger,
    schemaList: state.tables.schemaList,
  };
};

const addTriggerConnector = connect => connect(mapStateToProps)(AddTrigger);

export default addTriggerConnector;
