import PropTypes from 'prop-types';
import React, { Component } from 'react';
import AceEditor from 'react-ace';
import TableHeader from '../TableCommon/TableHeader';
import { deleteTrigger } from '../EventActions';
import _push from '../push';

class Settings extends Component {
  render() {
    const {
      triggerName,
      triggerList,
      migrationMode,
      count,
      dispatch,
    } = this.props;

    const styles = require('../TableCommon/Table.scss');
    let triggerSchema = triggerList.filter(
      elem => elem.name === triggerName
    )[0];
    triggerSchema = triggerSchema ? triggerSchema : {};

    const triggerConfiguration =
      ('configuration' in triggerSchema && triggerSchema.configuration) || {};

    const webhookConf =
      ('webhook' in triggerConfiguration && triggerConfiguration.webhook) ||
      ('webhook_from_env' in triggerConfiguration &&
        '<' + triggerConfiguration.webhook_from_env + '>') ||
      triggerSchema.webhook;
    let retryInterval = '';
    let noOfRetries = '';
    if ('retry_conf' in triggerConfiguration) {
      retryInterval = triggerConfiguration.retry_conf.interval_sec;
      noOfRetries = triggerConfiguration.retry_conf.num_retries;
    } else {
      retryInterval = triggerSchema.retry_interval;
      noOfRetries = triggerSchema.num_retries;
    }
    const headers =
      ('headers' in triggerConfiguration && triggerConfiguration.headers) ||
      ('headers' in triggerSchema && triggerSchema.headers) ||
      [];
    const definition =
      ('definition' in triggerConfiguration &&
        triggerConfiguration.definition) ||
      triggerSchema.definition;

    const handleDeleteTrigger = e => {
      e.preventDefault();
      const isOk = confirm('Are you sure?');
      if (isOk) {
        dispatch(deleteTrigger(triggerName));
      }
    };

    const handleModifyTrigger = () => {
      dispatch(_push(`/manage/triggers/${triggerName}/modify`));
    };

    return (
      <div className={styles.container + ' container-fluid'}>
        <TableHeader
          count={count}
          dispatch={dispatch}
          triggerName={triggerName}
          tabName="settings"
          migrationMode={migrationMode}
        />
        <br />
        <div>
          <div className={styles.settingsSection}>
            <table className="table table-striped table-bordered">
              <thead />
              <tbody>
                <tr>
                  <td>Webhook URL</td>
                  <td>{webhookConf}</td>
                </tr>
                <tr>
                  <td>Table</td>
                  <td>{triggerSchema.table_name}</td>
                </tr>
                <tr>
                  <td>Schema</td>
                  <td>{triggerSchema.schema_name}</td>
                </tr>
                <tr>
                  <td>Event Type</td>
                  <td>{triggerSchema.type}</td>
                </tr>
                <tr>
                  <td>Number of Retries</td>
                  <td>{noOfRetries}</td>
                </tr>
                <tr>
                  <td>Retry Interval</td>
                  <td>
                    {retryInterval} {retryInterval > 1 ? 'seconds' : 'second'}
                  </td>
                </tr>
                <tr>
                  <td>Headers</td>
                  <td>
                    <AceEditor
                      mode="json"
                      theme="github"
                      name="headers"
                      value={JSON.stringify(headers, null, 4)}
                      minLines={4}
                      maxLines={100}
                      width="100%"
                      showPrintMargin={false}
                      showGutter={false}
                      readOnly
                    />
                  </td>
                </tr>
                <tr>
                  <td>Operation / Columns</td>
                  <td>
                    <AceEditor
                      mode="json"
                      theme="github"
                      name="payload"
                      value={JSON.stringify(definition, null, 4)}
                      minLines={4}
                      maxLines={100}
                      width="100%"
                      showPrintMargin={false}
                      showGutter={false}
                      readOnly
                    />
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
          <div className={styles.add_mar_bottom}>
            <button
              onClick={handleModifyTrigger}
              className={`btn ${styles.yellow_button} ${
                styles.triggerSettingsModifyButton
              }`}
              data-test="modify-trigger"
            >
              Modify Trigger
            </button>
            <button
              onClick={handleDeleteTrigger}
              className={'btn btn-sm btn-danger'}
              data-test="delete-trigger"
            >
              Delete Trigger
            </button>
          </div>
        </div>
        <br />
        <br />
      </div>
    );
  }
}

Settings.propTypes = {
  tableName: PropTypes.string.isRequired,
  triggerList: PropTypes.array,
  migrationMode: PropTypes.bool.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.triggers,
    triggerName: ownProps.params.trigger,
    migrationMode: state.main.migrationMode,
    currentSchema: state.tables.currentSchema,
  };
};

const settingsConnector = connect => connect(mapStateToProps)(Settings);

export default settingsConnector;
