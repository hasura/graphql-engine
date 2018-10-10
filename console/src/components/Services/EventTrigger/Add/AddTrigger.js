import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';
import * as tooltip from './Tooltips';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import {
  setTriggerName,
  setTableName,
  setSchemaName,
  setWebhookURL,
  setRetryNum,
  setRetryInterval,
  operationToggleColumn,
  operationToggleAllColumns,
  setOperationSelection,
  setDefaults,
} from './AddActions';
import { showErrorNotification } from '../Notification';
import { createTrigger } from './AddActions';
import { fetchTableListBySchema } from './AddActions';

class AddTrigger extends Component {
  constructor(props) {
    super(props);
    this.props.dispatch(fetchTableListBySchema('public'));
    this.state = { advancedExpanded: false };
  }
  componentDidMount() {
    // set defaults
    this.props.dispatch(setDefaults());
  }
  componentWillUnmount() {
    // set defaults
    this.props.dispatch(setDefaults());
  }

  submitValidation(e) {
    // validations
    e.preventDefault();
    let isValid = true;
    let errorMsg = '';
    let customMsg = '';
    if (this.props.triggerName === '') {
      isValid = false;
      errorMsg = 'Trigger name cannot be empty';
      customMsg = 'Trigger name cannot be empty. Please add a name';
    } else if (!this.props.tableName) {
      isValid = false;
      errorMsg = 'Table cannot be empty';
      customMsg = 'Please select a table name';
    } else if (this.props.webhookURL === '') {
      isValid = false;
      errorMsg = 'Webhook URL cannot be empty';
      customMsg = 'Webhook URL cannot be empty. Please add a valid URL';
    } else if (this.props.retryConf) {
      if (isNaN(parseInt(this.props.retryConf.num_retries, 10))) {
        isValid = false;
        errorMsg = 'Number of retries is not valid';
        customMsg = 'Numer of retries cannot be empty and can only be numbers';
      }
      if (isNaN(parseInt(this.props.retryConf.interval_sec, 10))) {
        isValid = false;
        errorMsg = 'Retry interval is not valid';
        customMsg = 'Retry interval cannot be empty and can only be numbers';
      }
    } else if (this.props.selectedOperations.insert) {
      // check if columns are selected.
      if (this.props.operations.insert.length === 0) {
        isValid = false;
        errorMsg = 'No columns selected for insert operation';
        customMsg =
          'Please select a minimum of one column for insert operation';
      }
    } else if (this.props.selectedOperations.update) {
      // check if columns are selected.
      if (this.props.operations.update.length === 0) {
        isValid = false;
        errorMsg = 'No columns selected for update operation';
        customMsg =
          'Please select a minimum of one column for update operation';
      }
    }
    if (isValid) {
      this.props.dispatch(createTrigger());
    } else {
      this.props.dispatch(
        showErrorNotification('Error creating trigger!', errorMsg, '', {
          custom: customMsg,
        })
      );
    }
  }
  toggleAdvanced() {
    this.setState({ advancedExpanded: !this.state.advancedExpanded });
  }
  render() {
    const {
      tableName,
      tableListBySchema,
      schemaName,
      schemaList,
      selectedOperations,
      operations,
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
      const tableSchema = tableListBySchema.find(
        t => t.table_name === e.target.value
      );

      const columns = [];
      if (tableSchema) {
        tableSchema.columns.map(colObj => {
          const column = colObj.column_name;
          columns.push(column);
        });
      }
      dispatch(operationToggleAllColumns(columns));
    };

    const handleOperationSelection = e => {
      dispatch(setOperationSelection(e.target.value, e.target.checked));
    };

    const getColumnList = type => {
      const dispatchToggleColumn = e => {
        const column = e.target.value;
        dispatch(operationToggleColumn(column, type));
      };
      const tableSchema = tableListBySchema.find(
        t => t.table_name === tableName
      );

      if (tableSchema) {
        return tableSchema.columns.map((colObj, i) => {
          const column = colObj.column_name;
          const checked = operations[type]
            ? operations[type].includes(column)
            : false;

          const isDisabled = false;
          const inputHtml = (
            <input
              type="checkbox"
              checked={checked}
              value={column}
              onChange={dispatchToggleColumn}
              disabled={isDisabled}
            />
          );
          return (
            <div
              key={i}
              className={styles.display_inline + ' ' + styles.add_mar_right}
            >
              <div className="checkbox">
                <label>
                  {inputHtml}
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
          <form onSubmit={this.submitValidation.bind(this)}>
            <div
              className={`${styles.addCol} col-xs-12 ${
                styles.padd_left_remove
              }`}
            >
              <h4 className={styles.subheading_text}>
                Trigger Name &nbsp; &nbsp;
                <OverlayTrigger
                  placement="right"
                  overlay={tooltip.triggerNameDescription}
                >
                  <i className="fa fa-question-circle" aria-hidden="true" />
                </OverlayTrigger>{' '}
              </h4>
              <input
                type="text"
                data-test="trigger-name"
                placeholder="trigger_name"
                required
                pattern="^\w+$"
                className={`${styles.tableNameInput} form-control`}
                onChange={e => {
                  dispatch(setTriggerName(e.target.value));
                }}
              />
              <hr />
              <h4 className={styles.subheading_text}>
                Schema/Table &nbsp; &nbsp;
                <OverlayTrigger
                  placement="right"
                  overlay={tooltip.postgresDescription}
                >
                  <i className="fa fa-question-circle" aria-hidden="true" />
                </OverlayTrigger>{' '}
              </h4>
              <select
                onChange={updateTableList}
                data-test="select-schema"
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
                data-test="select-table"
                required
                className={
                  styles.selectTrigger + ' form-control ' + styles.add_mar_left
                }
              >
                <option value="">Select table</option>
                {tableListBySchema.map(t => {
                  if (t.detail.table_type === 'BASE TABLE') {
                    return (
                      <option key={t.table_name} value={t.table_name}>
                        {t.table_name}
                      </option>
                    );
                  }
                })}
              </select>
              <hr />
              <div
                className={
                  styles.add_mar_bottom + ' ' + styles.selectOperations
                }
              >
                <h4 className={styles.subheading_text}>
                  Operations &nbsp; &nbsp;
                  <OverlayTrigger
                    placement="right"
                    overlay={tooltip.operationsDescription}
                  >
                    <i className="fa fa-question-circle" aria-hidden="true" />
                  </OverlayTrigger>{' '}
                </h4>
                <div className={styles.display_inline}>
                  <label>
                    <input
                      onChange={handleOperationSelection}
                      data-test="insert-operation"
                      className={
                        styles.display_inline + ' ' + styles.add_mar_right
                      }
                      type="checkbox"
                      value="insert"
                      checked={selectedOperations.insert}
                    />
                    Insert
                  </label>
                </div>
                <div
                  className={styles.display_inline + ' ' + styles.add_mar_left}
                >
                  <label>
                    <input
                      onChange={handleOperationSelection}
                      data-test="update-operation"
                      className={
                        styles.display_inline + ' ' + styles.add_mar_right
                      }
                      type="checkbox"
                      value="update"
                      checked={selectedOperations.update}
                    />
                    Update
                  </label>
                </div>
                <div
                  className={styles.display_inline + ' ' + styles.add_mar_left}
                >
                  <label>
                    <input
                      onChange={handleOperationSelection}
                      data-test="delete-operation"
                      className={
                        styles.display_inline + ' ' + styles.add_mar_right
                      }
                      type="checkbox"
                      value="delete"
                      checked={selectedOperations.delete}
                    />
                    Delete
                  </label>
                </div>
              </div>
              <hr />
              <div className={styles.add_mar_bottom}>
                <h4 className={styles.subheading_text}>
                  Webhook URL &nbsp; &nbsp;
                  <OverlayTrigger
                    placement="right"
                    overlay={tooltip.webhookUrlDescription}
                  >
                    <i className="fa fa-question-circle" aria-hidden="true" />
                  </OverlayTrigger>{' '}
                </h4>
                <input
                  type="url"
                  required
                  data-test="webhook"
                  placeholder="webhook url"
                  className={`${styles.tableNameInput} form-control`}
                  onChange={e => {
                    dispatch(setWebhookURL(e.target.value));
                  }}
                />
              </div>
              <hr />
              <button
                onClick={this.toggleAdvanced.bind(this)}
                data-test="advanced-settings"
                type="button"
                className={'btn btn-default ' + styles.advancedToggleBtn}
              >
                Advanced Settings
                {this.state.advancedExpanded ? (
                  <i className={'fa fa-arrow-up'} />
                ) : (
                  <i className={'fa fa-arrow-down'} />
                )}
              </button>
              {this.state.advancedExpanded ? (
                <div
                  className={
                    styles.advancedOperations +
                    ' ' +
                    styles.add_mar_bottom +
                    ' ' +
                    styles.add_mar_top
                  }
                >
                  {tableName ? (
                    <div>
                      <h4 className={styles.subheading_text}>
                        Advanced - Operation/Columns &nbsp; &nbsp;
                        <OverlayTrigger
                          placement="right"
                          overlay={tooltip.advancedOperationDescription}
                        >
                          <i
                            className="fa fa-question-circle"
                            aria-hidden="true"
                          />
                        </OverlayTrigger>{' '}
                      </h4>
                      <div>
                        <div>
                          <label>
                            <input
                              onChange={handleOperationSelection}
                              className={
                                styles.display_inline +
                                ' ' +
                                styles.add_mar_right
                              }
                              type="checkbox"
                              value="insert"
                              checked={selectedOperations.insert}
                            />
                            Insert
                          </label>
                        </div>
                        {getColumnList('insert')}
                      </div>
                      <hr />
                      <div>
                        <div>
                          <label>
                            <input
                              onChange={handleOperationSelection}
                              className={
                                styles.display_inline +
                                ' ' +
                                styles.add_mar_right
                              }
                              type="checkbox"
                              value="update"
                              checked={selectedOperations.update}
                            />
                            Update
                          </label>
                        </div>
                        {getColumnList('update')}
                      </div>
                      <hr />
                      <div>
                        <div>
                          <label>
                            <input
                              onChange={handleOperationSelection}
                              className={
                                styles.display_inline +
                                ' ' +
                                styles.add_mar_right
                              }
                              type="checkbox"
                              value="delete"
                              checked={selectedOperations.delete}
                            />
                            Delete
                          </label>
                        </div>
                        {getColumnList('delete')}
                      </div>
                    </div>
                  ) : null}
                  <div
                    className={styles.add_mar_bottom + ' ' + styles.add_mar_top}
                  >
                    <h4 className={styles.subheading_text}>Retry Logic</h4>
                    <div
                      className={
                        styles.display_inline + ' ' + styles.retrySection
                      }
                    >
                      <label
                        className={
                          styles.add_mar_right + ' ' + styles.retryLabel
                        }
                      >
                        Number of retries (default: 0)
                      </label>
                      <input
                        onChange={e => {
                          dispatch(setRetryNum(e.target.value));
                        }}
                        data-test="no-of-retries"
                        className={styles.display_inline + ' form-control'}
                        type="text"
                        placeholder="no of retries"
                      />
                    </div>
                    <div
                      className={
                        styles.display_inline + ' ' + styles.retrySection
                      }
                    >
                      <label
                        className={
                          styles.add_mar_right + ' ' + styles.retryLabel
                        }
                      >
                        Retry Interval in seconds (default: 10)
                      </label>
                      <input
                        onChange={e => {
                          dispatch(setRetryInterval(e.target.value));
                        }}
                        data-test="interval-seconds"
                        className={styles.display_inline + ' form-control'}
                        type="text"
                        placeholder="interval time in seconds"
                      />
                    </div>
                  </div>
                </div>
              ) : null}
              <hr />
              <button
                type="submit"
                className={`btn ${styles.yellow_button}`}
                data-test="trigger-create"
              >
                {createBtnText}
              </button>
            </div>
          </form>
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
  selectedOperations: PropTypes.object,
  operations: PropTypes.object,
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
