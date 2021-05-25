import React, { Dispatch, useState } from 'react';

import { ConnectDBActions, ConnectDBState } from './state';
import { LabeledInput } from '../../../Common/LabeledInput';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { getSupportedDrivers } from '../../../../dataSources';

import styles from './DataSources.scss';

export interface ConnectionSettingsFormProps {
  // Connect DB State Props
  connectionDBState: ConnectDBState;
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
}

const ConnectionSettingsForm: React.FC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => {
  const [currentConnectionParamState, toggleConnectionParamState] = useState(
    false
  );
  const toggleConnectionParams = (value: boolean) => () => {
    toggleConnectionParamState(value);
  };

  return (
    <>
      {getSupportedDrivers('connectDbForm.connectionSettings').includes(
        connectionDBState.dbType
      ) ? (
        <div className={styles.connection_settings_layout}>
          <div className={styles.connection_settings_header}>
            <a
              href="#"
              style={{ textDecoration: 'none' }}
              onClick={toggleConnectionParams(!currentConnectionParamState)}
            >
              {currentConnectionParamState ? (
                <i className="fa fa-caret-down" />
              ) : (
                <i className="fa fa-caret-right" />
              )}
              {'  '}
              Connection Settings
            </a>
          </div>
          {currentConnectionParamState ? (
            <div className={styles.connection_settings_form}>
              <div className={styles.connection_settings_input_layout}>
                <LabeledInput
                  label="Max Connections"
                  type="number"
                  className={`form-control ${styles.connnection_settings_form_input}`}
                  placeholder="50"
                  value={
                    connectionDBState.connectionSettings?.max_connections ??
                    undefined
                  }
                  onChange={e =>
                    connectionDBStateDispatch({
                      type: 'UPDATE_MAX_CONNECTIONS',
                      data: e.target.value,
                    })
                  }
                  min="0"
                  boldlabel
                  data-test="max-connections"
                />
              </div>
              <div className={styles.connection_settings_input_layout}>
                <LabeledInput
                  label="Idle Timeout"
                  type="number"
                  className={`form-control ${styles.connnection_settings_form_input}`}
                  placeholder="180"
                  value={
                    connectionDBState.connectionSettings?.idle_timeout ??
                    undefined
                  }
                  onChange={e =>
                    connectionDBStateDispatch({
                      type: 'UPDATE_IDLE_TIMEOUT',
                      data: e.target.value,
                    })
                  }
                  min="0"
                  boldlabel
                  data-test="idle-timeout"
                />
              </div>
              {getSupportedDrivers('connectDbForm.retries').includes(
                connectionDBState.dbType
              ) ? (
                <div className={styles.connection_settings_input_layout}>
                  <LabeledInput
                    label="Retries"
                    type="number"
                    className={`form-control ${styles.connnection_settings_form_input}`}
                    placeholder="1"
                    value={
                      connectionDBState.connectionSettings?.retries ?? undefined
                    }
                    onChange={e =>
                      connectionDBStateDispatch({
                        type: 'UPDATE_RETRIES',
                        data: e.target.value,
                      })
                    }
                    min="0"
                    boldlabel
                    data-test="retries"
                  />
                </div>
              ) : null}
              {getSupportedDrivers(
                'connectDbForm.prepared_statements'
              ).includes(connectionDBState.dbType) ? (
                <div
                  className={`${styles.add_mar_bottom_mid} ${styles.checkbox_margin_top}`}
                >
                  <label>
                    <input
                      type="checkbox"
                      checked={connectionDBState.preparedStatements}
                      onChange={e => {
                        connectionDBStateDispatch({
                          type: 'UPDATE_PREPARED_STATEMENTS',
                          data: e.target.checked,
                        });
                      }}
                    />{' '}
                    &nbsp;
                    <b>Use Prepared Statements</b>
                    <Tooltip message="Prepared statements are disabled by default" />
                  </label>
                </div>
              ) : null}
            </div>
          ) : null}
        </div>
      ) : null}
    </>
  );
};

export default ConnectionSettingsForm;
