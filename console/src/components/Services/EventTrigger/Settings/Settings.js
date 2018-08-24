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
        <div className={'container-fluid'}>
          <div className={styles.settingsSection}>
            <div className={styles.add_mar_bottom}>
              <div className={styles.display_inline + ' ' + styles.settingsRow}>
                URL
              </div>
              <div className={styles.display_inline}>
                {triggerSchema.webhook}
              </div>
            </div>
            <div className={styles.add_mar_bottom}>
              <div className={styles.display_inline + ' ' + styles.settingsRow}>
                Table Name
              </div>
              <div className={styles.display_inline}>
                {triggerSchema.table_name}
              </div>
            </div>
            <div className={styles.add_mar_bottom}>
              <div className={styles.display_inline + ' ' + styles.settingsRow}>
                Schema
              </div>
              <div className={styles.display_inline}>
                {triggerSchema.schema_name}
              </div>
            </div>
            <div className={styles.add_mar_bottom}>
              <div className={styles.display_inline + ' ' + styles.settingsRow}>
                Event Type
              </div>
              <div className={styles.display_inline}>{triggerSchema.type}</div>
            </div>
            <div className={styles.add_mar_bottom}>
              <div className={styles.display_inline + ' ' + styles.settingsRow}>
                Retry Number
              </div>
              <div className={styles.display_inline + ' ' + styles.settingsRow}>
                {retryConf.num_retries}
              </div>
            </div>
            <div className={styles.add_mar_bottom}>
              <div className={styles.display_inline + ' ' + styles.settingsRow}>
                Retry Interval
              </div>
              <div className={styles.display_inline}>
                {retryConf.interval_seconds}
              </div>
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
