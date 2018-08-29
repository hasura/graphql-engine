import PropTypes from 'prop-types';
import React, { Component } from 'react';
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

    let retryConf = {};
    if (triggerSchema.retry_conf) {
      if (triggerSchema.retry_conf[0]) {
        retryConf = triggerSchema.retry_conf[0];
      } else {
        retryConf = triggerSchema.retry_conf;
      }
    }

    const handleDeleteTrigger = () => {
      dispatch(deleteTrigger(triggerName));
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
                  <td>{retryConf.num_retries}</td>
                </tr>
                <tr>
                  <td>Retry Interval</td>
                  <td>
                    {retryConf.interval_seconds}{' '}
                    {retryConf.interval_seconds > 1 ? 'seconds' : 'second'}
                  </td>
                </tr>
                <tr>
                  <td>Operation / Columns</td>
                  <td>{JSON.stringify(triggerSchema.definition, null, 4)}</td>
                </tr>
              </tbody>
            </table>
          </div>
          <div className={styles.add_mar_bottom}>
            <button
              onClick={handleDeleteTrigger}
              className={'btn btn-sm btn-danger'}
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
