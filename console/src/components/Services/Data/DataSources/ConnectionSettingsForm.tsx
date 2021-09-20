import React, { Dispatch, useState } from 'react';

import { ConnectDBActions, ConnectDBState } from './state';
import { LabeledInput } from '../../../Common/LabeledInput';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { getSupportedDrivers } from '../../../../dataSources';

import styles from './DataSources.scss';
import {
  SSLModeOptions,
  IsolationLevelOptions,
} from '../../../../metadata/types';

export interface ConnectionSettingsFormProps {
  // Connect DB State Props
  connectionDBState: ConnectDBState;
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
}

const ConnectionSettingsForm: React.FC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => {
  const [currentConnectionParamState, setConnectionParamState] = useState(
    false
  );
  const [certificateSettingsState, setCertificateSettingsState] = useState(
    false
  );

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
              onClick={() =>
                setConnectionParamState(!currentConnectionParamState)
              }
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
                  tooltipText="Maximum number of connections to be kept in the pool"
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
                  tooltipText="The idle timeout (in seconds) per connection"
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
                    tooltipText="Number of retries to perform"
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
              {getSupportedDrivers('connectDbForm.pool_timeout').includes(
                connectionDBState.dbType
              ) ? (
                <div className={styles.connection_settings_input_layout}>
                  <LabeledInput
                    label="Pool Timeout"
                    tooltipText="Maximum time (in seconds) to wait while acquiring a Postgres connection from the pool"
                    type="number"
                    className={`form-control ${styles.connnection_settings_form_input}`}
                    placeholder="360"
                    value={
                      connectionDBState.connectionSettings?.pool_timeout ??
                      undefined
                    }
                    onChange={e =>
                      connectionDBStateDispatch({
                        type: 'UPDATE_POOL_TIMEOUT',
                        data: e.target.value,
                      })
                    }
                    min="0"
                    boldlabel
                    data-test="pool-timeout"
                  />
                </div>
              ) : null}
              {getSupportedDrivers(
                'connectDbForm.connection_lifetime'
              ).includes(connectionDBState.dbType) ? (
                <div className={styles.connection_settings_input_layout}>
                  <LabeledInput
                    label="Connection Lifetime"
                    tooltipText="Time (in seconds) from connection creation after which the connection should be destroyed and a new one created. A value of 0 indicates we should never destroy an active connection. If 0 is passed, memory from large query results may not be reclaimed."
                    type="number"
                    className={`form-control ${styles.connnection_settings_form_input}`}
                    placeholder="600"
                    value={
                      connectionDBState.connectionSettings
                        ?.connection_lifetime ?? undefined
                    }
                    onChange={e =>
                      connectionDBStateDispatch({
                        type: 'UPDATE_CONNECTION_LIFETIME',
                        data: e.target.value,
                      })
                    }
                    min="0"
                    boldlabel
                    data-test="connection-lifetime"
                  />
                </div>
              ) : null}
              {getSupportedDrivers('connectDbForm.isolation_level').includes(
                connectionDBState.dbType
              ) && (
                <div className={styles.connection_settings_input_layout}>
                  <label>
                    <b>Isolation Level</b>
                    <Tooltip message="The transaction isolation level in which the queries made to the source will be run" />
                  </label>
                  <select
                    className={`form-control ${styles.connnection_settings_form_input}`}
                    onChange={e =>
                      connectionDBStateDispatch({
                        type: 'UPDATE_ISOLATION_LEVEL',
                        data: e.target.value as IsolationLevelOptions,
                      })
                    }
                    value={connectionDBState.isolationLevel}
                  >
                    <option value="read-committed">read-committed</option>
                    <option value="repeatable-read">repeatable-read</option>
                    <option value="serializable">serializable</option>
                  </select>
                </div>
              )}
              {getSupportedDrivers(
                'connectDbForm.prepared_statements'
              ).includes(connectionDBState.dbType) ? (
                <div
                  className={`${styles.add_mar_bottom_mid} ${styles.checkbox_margin_top}`}
                >
                  <label className="inline-flex items-center">
                    <input
                      type="checkbox"
                      checked={connectionDBState.preparedStatements}
                      className="legacy-input-fix"
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
              {getSupportedDrivers('connectDbForm.ssl_certificates').includes(
                connectionDBState.dbType
              ) && (
                <div className={styles.add_mar_top}>
                  <a
                    href="#"
                    style={{ textDecoration: 'none' }}
                    onClick={() =>
                      setCertificateSettingsState(!certificateSettingsState)
                    }
                    className={styles.connection_settings_header}
                  >
                    {certificateSettingsState ? (
                      <i className="fa fa-caret-down" />
                    ) : (
                      <i className="fa fa-caret-right" />
                    )}
                    {'  '}
                    SSL Certificates Settings
                  </a>
                  <div className={styles.text_muted}>
                    Certificates will be loaded from{' '}
                    <a href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database">
                      environment variables
                    </a>
                  </div>
                  {certificateSettingsState ? (
                    <div className={styles.connection_settings_form}>
                      <div className={styles.connection_settings_input_layout}>
                        <label>
                          <b>SSL Mode</b>
                          <Tooltip message="SSL certificate verification mode" />
                        </label>
                        <select
                          className="form-control"
                          onChange={e =>
                            connectionDBStateDispatch({
                              type: 'UPDATE_SSL_MODE',
                              data:
                                (e.target.value as SSLModeOptions) || undefined,
                            })
                          }
                          value={connectionDBState.sslConfiguration?.sslmode}
                        >
                          <option value="">--</option>
                          <option value="disable">disable</option>
                          <option value="verify-ca">verify-ca</option>
                          <option value="verify-full">verify-full</option>
                        </select>
                      </div>
                      <LabeledInput
                        label="SSL Root Certificate"
                        type="text"
                        placeholder="SSL_ROOT_CERT"
                        value={
                          connectionDBState.sslConfiguration?.sslrootcert
                            ?.from_env ?? undefined
                        }
                        onChange={e =>
                          connectionDBStateDispatch({
                            type: 'UPDATE_SSL_ROOT_CERT',
                            data: e.target.value,
                          })
                        }
                      />
                      <LabeledInput
                        label="SSL Certificate"
                        type="text"
                        placeholder="SSL_CERT"
                        value={
                          connectionDBState.sslConfiguration?.sslcert
                            ?.from_env ?? undefined
                        }
                        onChange={e =>
                          connectionDBStateDispatch({
                            type: 'UPDATE_SSL_CERT',
                            data: e.target.value,
                          })
                        }
                      />
                      <LabeledInput
                        label="SSL Key"
                        type="text"
                        placeholder="SSL_KEY"
                        value={
                          connectionDBState.sslConfiguration?.sslkey
                            ?.from_env ?? undefined
                        }
                        onChange={e =>
                          connectionDBStateDispatch({
                            type: 'UPDATE_SSL_KEY',
                            data: e.target.value,
                          })
                        }
                      />
                      <LabeledInput
                        label="SSL Password"
                        type="text"
                        className="form-control"
                        placeholder="SSL_PASSWORD"
                        value={
                          connectionDBState.sslConfiguration?.sslpassword
                            ?.from_env ?? undefined
                        }
                        onChange={e =>
                          connectionDBStateDispatch({
                            type: 'UPDATE_SSL_PASSWORD',
                            data: e.target.value,
                          })
                        }
                      />
                    </div>
                  ) : null}
                </div>
              )}
            </div>
          ) : null}
        </div>
      ) : null}
    </>
  );
};

export default ConnectionSettingsForm;
