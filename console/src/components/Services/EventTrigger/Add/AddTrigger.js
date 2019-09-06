import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';
import * as tooltip from './Tooltips';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Button from '../../../Common/Button/Button';
import Operations from './Operations';

import {
  removeHeader,
  setHeaderKey,
  setHeaderValue,
  setHeaderType,
  addHeader,
  setTriggerName,
  setTableName,
  setSchemaName,
  setWebhookURL,
  setRetryNum,
  setRetryInterval,
  setRetryTimeout,
  operationToggleColumn,
  operationToggleAllColumns,
  setOperationSelection,
  setDefaults,
  UPDATE_WEBHOOK_URL_TYPE,
  loadTableList,
} from './AddActions';
import { listDuplicate } from '../../../../utils/data';
import { showErrorNotification } from '../../Common/Notification';
import { createTrigger } from './AddActions';

import DropdownButton from '../../../Common/DropdownButton/DropdownButton';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';

class AddTrigger extends Component {
  constructor(props) {
    super(props);
    this.props.dispatch(loadTableList('public'));
  }

  componentDidMount() {
    // set defaults
    this.props.dispatch(setDefaults());
  }

  componentWillUnmount() {
    // set defaults
    this.props.dispatch(setDefaults());
  }

  updateWebhookUrlType(e) {
    const field = e.target.getAttribute('value');
    if (field === 'env' || field === 'url') {
      this.props.dispatch({ type: UPDATE_WEBHOOK_URL_TYPE, data: field });
      this.props.dispatch(setWebhookURL(''));
    }
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
      const iNumRetries =
        this.props.retryConf.num_retries === ''
          ? 0
          : parseInt(this.props.retryConf.num_retries, 10);
      const iRetryInterval =
        this.props.retryConf.interval_sec === ''
          ? 10
          : parseInt(this.props.retryConf.interval_sec, 10);
      const iTimeout =
        this.props.retryConf.timeout_sec === ''
          ? 60
          : parseInt(this.props.retryConf.timeout_sec, 10);

      if (iNumRetries < 0 || isNaN(iNumRetries)) {
        isValid = false;
        errorMsg = 'Number of retries is not valid';
        customMsg = 'Numer of retries must be a non-negative number';
      }
      if (iRetryInterval <= 0 || isNaN(iRetryInterval)) {
        isValid = false;
        errorMsg = 'Retry interval is not valid';
        customMsg = 'Retry interval must be a postiive number';
      }
      if (isNaN(iTimeout) || iTimeout <= 0) {
        isValid = false;
        errorMsg = 'Timeout is not valid';
        customMsg = 'Timeout must be a positive number';
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
    } else if (this.props.headers.length === 1) {
      if (this.props.headers[0].key !== '') {
        // let the default value through and ignore it while querying?
        // Need a better method
        if (this.props.headers[0].type === '') {
          isValid = false;
          errorMsg = 'No type selected for trigger header';
          customMsg = 'Please select a type for the trigger header';
        }
      }
    } else if (this.props.headers.length > 1) {
      // repitition check
      const repeatList = listDuplicate(
        this.props.headers.map(header => header.key)
      );
      if (repeatList.length > 0) {
        isValid = false;
        errorMsg = 'Duplicate entries in trigger headers';
        customMsg = `You have the following column names repeated: [${repeatList}]`;
      }
      // Check for empty header keys and key/value validation?
    }
    if (isValid) {
      this.props.dispatch(createTrigger());
    } else {
      this.props.dispatch(
        showErrorNotification('Error creating trigger!', errorMsg, {
          custom: customMsg,
        })
      );
    }
  }

  render() {
    const {
      tableName,
      allSchemas,
      schemaName,
      schemaList,
      selectedOperations,
      operations,
      dispatch,
      ongoingRequest,
      lastError,
      lastSuccess,
      internalError,
      headers,
      webhookURL,
      webhookUrlType,
      enableManual,
    } = this.props;

    const styles = require('../TableCommon/EventTable.scss');

    let createBtnText = 'Create Event Trigger';
    if (ongoingRequest) {
      createBtnText = 'Creating...';
    } else if (lastError) {
      createBtnText = 'Creating Failed. Try again';
    } else if (internalError) {
      createBtnText = 'Creating Failed. Try again';
    } else if (lastSuccess) {
      createBtnText = 'Created! Redirecting...';
    }

    const handleOperationSelection = e => {
      dispatch(setOperationSelection(e.target.value));
    };

    const updateTableList = e => {
      const selectedSchemaName = e.target.value;
      dispatch(setSchemaName(selectedSchemaName));
      dispatch(loadTableList(selectedSchemaName));
    };

    const updateTableSelection = e => {
      const selectedTableName = e.target.value;
      dispatch(setTableName(selectedTableName));
      const tableSchema = allSchemas.find(
        t => t.table_name === selectedTableName && t.table_schema === schemaName
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

    const getColumnList = type => {
      const dispatchToggleColumn = e => {
        const column = e.target.value;
        dispatch(operationToggleColumn(column, type));
      };
      const tableSchema = allSchemas.find(
        t => t.table_name === tableName && t.table_schema === schemaName
      );

      if (!tableSchema) {
        return <i>Select a table first to get column list</i>;
      }

      return tableSchema.columns.map((colObj, i) => {
        const column = colObj.column_name;
        const columnDataType = colObj.udt_name;
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
          <div key={i} className={styles.padd_remove + ' col-md-4'}>
            <div className={'checkbox '}>
              <label>
                {inputHtml}
                {column}
                <small> ({columnDataType})</small>
              </label>
            </div>
          </div>
        );
      });
    };

    const advancedColumnSection = (
      <div>
        <h4 className={styles.subheading_text}>
          Listen columns for update &nbsp; &nbsp;
          <OverlayTrigger
            placement="right"
            overlay={tooltip.advancedOperationDescription}
          >
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>{' '}
        </h4>
        {selectedOperations.update ? (
          <div className={styles.clear_fix + ' ' + styles.listenColumnWrapper}>
            {getColumnList('update')}
          </div>
        ) : (
          <div className={styles.clear_fix + ' ' + styles.listenColumnWrapper}>
            <i>Applicable only if update operation is selected.</i>
          </div>
        )}
      </div>
    );

    const headersList = headers.map((header, i) => {
      let removeIcon;
      if (i + 1 === headers.length) {
        removeIcon = <i className={`${styles.fontAwosomeClose}`} />;
      } else {
        removeIcon = (
          <i
            className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
            onClick={() => {
              dispatch(removeHeader(i));
            }}
          />
        );
      }
      return (
        <div key={i} className={`${styles.display_flex} form-group`}>
          <input
            type="text"
            className={`${styles.input} form-control ${styles.add_mar_right}`}
            value={header.key}
            placeholder="key"
            onChange={e => {
              dispatch(setHeaderKey(e.target.value, i));
            }}
            data-test={`header-${i}`}
          />
          <div className={styles.dropDownGroup}>
            <DropdownButton
              dropdownOptions={[
                { display_text: 'Value', value: 'static' },
                { display_text: 'From env var', value: 'env' },
              ]}
              title={
                (header.type === 'static' && 'Value') ||
                (header.type === 'env' && 'From env var') ||
                'Value'
              }
              dataKey={
                (header.type === 'static' && 'static') ||
                (header.type === 'env' && 'env')
              }
              title={header.type === 'env' ? 'From env var' : 'Value'}
              dataKey={header.type === 'env' ? 'env' : 'static'}
              onButtonChange={e => {
                dispatch(setHeaderType(e.target.getAttribute('value'), i));
              }}
              onInputChange={e => {
                dispatch(setHeaderValue(e.target.value, i));
                if (i + 1 === headers.length) {
                  dispatch(addHeader());
                }
              }}
              bsClass={styles.dropdown_button}
              inputVal={header.value}
              id={`header-value-${i}`}
              inputPlaceHolder={
                header.type === 'env' ? 'HEADER_FROM_ENV' : 'value'
              }
              testId={`header-value-${i}`}
            />
          </div>
          <div>{removeIcon}</div>
        </div>
      );
    });

    return (
      <div
        className={`${styles.addTablesBody} ${styles.clear_fix} ${
          styles.padd_left
        }`}
      >
        <Helmet title="Create Trigger - Events | Hasura" />
        <div className={styles.subHeader}>
          <h2 className={styles.heading_text}>Create a new event trigger</h2>
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
                pattern="^[A-Za-z]+[A-Za-z0-9_\\-]*$"
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
                {allSchemas.map(t => {
                  if (
                    t.table_schema === schemaName &&
                    t.table_type === 'BASE TABLE'
                  ) {
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
                <Operations
                  dispatch={dispatch}
                  enableManual={enableManual}
                  selectedOperations={selectedOperations}
                  handleOperationSelection={handleOperationSelection}
                />
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
                <div>
                  <div className={styles.dropdown_wrapper}>
                    <DropdownButton
                      dropdownOptions={[
                        { display_text: 'URL', value: 'url' },
                        { display_text: 'From env var', value: 'env' },
                      ]}
                      title={
                        (webhookUrlType === 'url' && 'URL') ||
                        (webhookUrlType === 'env' && 'From env var') ||
                        'Value'
                      }
                      dataKey={
                        (webhookUrlType === 'url' && 'url') ||
                        (webhookUrlType === 'env' && 'env')
                      }
                      onButtonChange={this.updateWebhookUrlType.bind(this)}
                      onInputChange={e => {
                        dispatch(setWebhookURL(e.target.value));
                      }}
                      required
                      bsClass={styles.dropdown_button}
                      inputVal={webhookURL}
                      id="webhook-url"
                      inputPlaceHolder={
                        (webhookUrlType === 'url' &&
                          'http://httpbin.org/post') ||
                        (webhookUrlType === 'env' && 'MY_WEBHOOK_URL')
                      }
                      testId="webhook"
                    />
                  </div>
                </div>
                <br />
                <small>
                  Note: Specifying the webhook URL via an environmental variable
                  is recommended if you have different URLs for multiple
                  environments.
                </small>
              </div>
              <hr />
              <CollapsibleToggle
                title={
                  <h4 className={styles.subheading_text}>Advanced Settings</h4>
                }
                testId="advanced-settings"
              >
                <div>
                  {advancedColumnSection}
                  <hr />
                  <div className={styles.add_mar_top}>
                    <h4 className={styles.subheading_text}>Retry Logic</h4>
                    <div className={styles.retrySection}>
                      <div className={`col-md-3 ${styles.padd_left_remove}`}>
                        <label
                          className={`${styles.add_mar_right} ${
                            styles.retryLabel
                          }`}
                        >
                          Number of retries (default: 0)
                        </label>
                      </div>
                      <div className={`col-md-6 ${styles.padd_left_remove}`}>
                        <input
                          onChange={e => {
                            dispatch(setRetryNum(e.target.value));
                          }}
                          data-test="no-of-retries"
                          className={`${styles.display_inline} form-control ${
                            styles.width300
                          }`}
                          type="text"
                          placeholder="no of retries"
                        />
                      </div>
                    </div>
                    <div className={styles.retrySection}>
                      <div className={`col-md-3 ${styles.padd_left_remove}`}>
                        <label
                          className={`${styles.add_mar_right} ${
                            styles.retryLabel
                          }`}
                        >
                          Retry Interval in seconds (default: 10)
                        </label>
                      </div>
                      <div className={`col-md-6 ${styles.padd_left_remove}`}>
                        <input
                          onChange={e => {
                            dispatch(setRetryInterval(e.target.value));
                          }}
                          data-test="interval-seconds"
                          className={`${styles.display_inline} form-control ${
                            styles.width300
                          }`}
                          type="text"
                          placeholder="interval time in seconds"
                        />
                      </div>
                    </div>
                    <div className={styles.retrySection}>
                      <div className={`col-md-3 ${styles.padd_left_remove}`}>
                        <label
                          className={`${styles.add_mar_right} ${
                            styles.retryLabel
                          }`}
                        >
                          Timeout in seconds (default: 60)
                        </label>
                      </div>
                      <div className={`col-md-6 ${styles.padd_left_remove}`}>
                        <input
                          onChange={e => {
                            dispatch(setRetryTimeout(e.target.value));
                          }}
                          data-test="timeout-seconds"
                          className={`${styles.display_inline} form-control ${
                            styles.width300
                          }`}
                          type="text"
                          placeholder="timeout in seconds"
                        />
                      </div>
                    </div>
                  </div>
                  <hr />
                  <div className={styles.add_mar_top}>
                    <h4 className={styles.subheading_text}>Headers</h4>
                    {headersList}
                  </div>
                </div>
              </CollapsibleToggle>
              <hr />
              <Button
                type="submit"
                color="yellow"
                size="sm"
                data-test="trigger-create"
              >
                {createBtnText}
              </Button>
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
  allSchemas: PropTypes.array.isRequired,
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
    allSchemas: state.tables.allSchemas,
    serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  };
};

const addTriggerConnector = connect => connect(mapStateToProps)(AddTrigger);

export default addTriggerConnector;
