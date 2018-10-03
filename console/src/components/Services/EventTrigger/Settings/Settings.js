import PropTypes from 'prop-types';
import React, { Component } from 'react';
import AceEditor from 'react-ace';
import TableHeader from '../TableCommon/TableHeader';
import { deleteTrigger } from '../EventActions';

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

    const handleDeleteTrigger = e => {
      e.preventDefault();
      const isOk = confirm('Are you sure?');
      if (isOk) {
        dispatch(deleteTrigger(triggerName));
      }
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
                  <td>{triggerSchema.webhook}</td>
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
                  <td>{triggerSchema.num_retries}</td>
                </tr>
                <tr>
                  <td>Retry Interval</td>
                  <td>
                    {triggerSchema.retry_interval}{' '}
                    {triggerSchema.retry_interval > 1 ? 'seconds' : 'second'}
                  </td>
                </tr>
                <tr>
                  <td>Operation / Columns</td>
                  <td>
                    <AceEditor
                      mode="json"
                      theme="github"
                      name="payload"
                      value={JSON.stringify(triggerSchema.definition, null, 4)}
                      minLines={4}
                      maxLines={100}
                      width="100%"
                      showPrintMargin={false}
                      showGutter={false}
                    />
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
          <div className={styles.add_mar_bottom}>
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
