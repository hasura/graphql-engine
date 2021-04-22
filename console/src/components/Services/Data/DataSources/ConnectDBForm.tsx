import React, { ChangeEvent, Dispatch, useState } from 'react';

import { ConnectDBActions, ConnectDBState, connectionTypes } from './state';
import { LabeledInput } from '../../../Common/LabeledInput';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { Driver } from '../../../../dataSources';
import { readFile } from './utils';

import styles from './DataSources.scss';

type ConnectDatabaseFormProps = {
  // Connect DB State Props
  connectionDBState: ConnectDBState;
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
  // Connection Type Props - for the Radio buttons
  updateConnectionTypeRadio: (e: ChangeEvent<HTMLInputElement>) => void;
  connectionTypeState: string;
  // Other Props
  isreadreplica?: boolean;
  title?: string;
};

export const connectionRadios = [
  {
    value: connectionTypes.CONNECTION_PARAMS,
    title: 'Connection Parameters',
    disableOnEdit: true,
  },
  {
    value: connectionTypes.DATABASE_URL,
    title: 'Database URL',
    disableOnEdit: false,
  },
  {
    value: connectionTypes.ENV_VAR,
    title: 'Environment Variable',
    disableOnEdit: true,
  },
];

const dbTypePlaceholders: Record<Driver, string> = {
  postgres: 'postgresql://username:password@hostname:5432/database',
  mssql:
    'Driver={ODBC Driver 17 for SQL Server};Server=serveraddress;Database=dbname;Uid=username;Pwd=password;',
  mysql: 'MySQL connection string',
  bigquery: 'SERVICE_ACCOUNT_KEY_FROM_ENV',
};

const defaultTitle = 'Connect Database Via';

const ConnectDatabaseForm: React.FC<ConnectDatabaseFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
  updateConnectionTypeRadio,
  connectionTypeState,
  isreadreplica = false,
  title,
}) => {
  const [currentConnectionParamState, toggleConnectionParamState] = useState(
    false
  );
  const toggleConnectionParams = (value: boolean) => () => {
    toggleConnectionParamState(value);
  };

  const handleFileUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files![0];
    const addFileQueries = (content: string) => {
      try {
        connectionDBStateDispatch({
          type: 'UPDATE_DB_BIGQUERY_SERVICE_ACCOUNT_FILE',
          data: content,
        });
      } catch (error) {
        console.log(error);
      }
    };

    readFile(file, addFileQueries);
  };

  return (
    <>
      <h4 className={`${styles.remove_pad_bottom} ${styles.connect_db_header}`}>
        {title ?? defaultTitle}
      </h4>
      <div
        className={styles.connect_db_radios}
        onChange={updateConnectionTypeRadio}
      >
        {connectionRadios.map(radioBtn => (
          <label
            key={`label-${radioBtn.title}`}
            className={styles.connect_db_radio_label}
          >
            <input
              type="radio"
              value={radioBtn.value}
              name={
                !isreadreplica
                  ? 'connection-type'
                  : 'connection-type-read-replica'
              }
              checked={connectionTypeState.includes(radioBtn.value)}
              defaultChecked={
                connectionTypeState === connectionTypes.DATABASE_URL
              }
              // disabled={
              //   isEditState === radioBtn.disableOnEdit && isEditState
              // }
            />
            {radioBtn.title}
          </label>
        ))}
      </div>
      <div className={styles.connect_form_layout}>
        {!isreadreplica && (
          <>
            <LabeledInput
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DISPLAY_NAME',
                  data: e.target.value,
                })
              }
              value={connectionDBState.displayName}
              label="Database Display Name"
              placeholder="database name"
              data-test="database-display-name"
            />
            <label
              key="Data Source Driver"
              className={styles.connect_db_input_label}
            >
              Data Source Driver
            </label>
            <select
              key="connect-db-type"
              value={connectionDBState.dbType}
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_DRIVER',
                  data: e.target.value as Driver,
                })
              }
              className={`form-control ${styles.connect_db_input_pad}`}
              data-test="database-type"
            >
              <option key="postgres" value="postgres">
                Postgres
              </option>
              <option key="mssql" value="mssql">
                MS Server
              </option>
              <option key="bigquery" value="bigquery">
                BigQuery
              </option>
            </select>
          </>
        )}
        {(connectionTypeState.includes(connectionTypes.DATABASE_URL) ||
          (connectionTypeState.includes(connectionTypes.CONNECTION_PARAMS) &&
            connectionDBState.dbType === 'mssql')) &&
        connectionDBState.dbType !== 'bigquery' ? (
          <LabeledInput
            label="Database URL"
            onChange={e =>
              connectionDBStateDispatch({
                type: 'UPDATE_DB_URL',
                data: e.target.value,
              })
            }
            value={connectionDBState.databaseURLState.dbURL}
            placeholder={dbTypePlaceholders[connectionDBState.dbType]}
            data-test="database-url"
            // disabled={isEditState}
          />
        ) : null}
        {connectionTypeState.includes(connectionTypes.ENV_VAR) &&
        connectionDBState.dbType !== 'bigquery' ? (
          <LabeledInput
            label="Environment Variable"
            placeholder="HASURA_GRAPHQL_DB_URL_FROM_ENV"
            onChange={e =>
              connectionDBStateDispatch({
                type: 'UPDATE_DB_URL_ENV_VAR',
                data: e.target.value,
              })
            }
            value={connectionDBState.envVarState.envVar}
            data-test="database-url-env"
          />
        ) : null}
        {(connectionTypeState.includes(connectionTypes.DATABASE_URL) ||
          connectionTypeState.includes(connectionTypes.CONNECTION_PARAMS) ||
          connectionTypeState.includes(connectionTypes.ENV_VAR)) &&
        connectionDBState.dbType === 'bigquery' ? (
          <>
            {connectionTypeState.includes(connectionTypes.ENV_VAR) ? (
              <LabeledInput
                label="Environment Variable"
                placeholder={dbTypePlaceholders[connectionDBState.dbType]}
                onChange={e =>
                  connectionDBStateDispatch({
                    type: 'UPDATE_DB_URL_ENV_VAR',
                    data: e.target.value,
                  })
                }
                value={connectionDBState.envVarState.envVar}
                data-test="service-account-env-var"
              />
            ) : (
              <div className={styles.add_mar_bottom_mid}>
                <div className={styles.add_mar_bottom_mid}>
                  <b>Service Account File:</b>
                  <Tooltip message="Service account key file for bigquery db" />
                </div>
                <input
                  type="file"
                  className={`form-control input-sm ${styles.inline_block}`}
                  onChange={handleFileUpload}
                />
              </div>
            )}
            <LabeledInput
              label="Project Id"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_BIGQUERY_PROJECT_ID',
                  data: e.target.value,
                })
              }
              value={connectionDBState.databaseURLState.projectId}
              placeholder="project_id"
              data-test="project-id"
            />
            <LabeledInput
              label="Datasets"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_BIGQUERY_DATASETS',
                  data: e.target.value,
                })
              }
              value={connectionDBState.databaseURLState.datasets}
              placeholder="dataset1, dataset2"
              data-test="datasets"
            />
          </>
        ) : null}
        {connectionTypeState.includes(connectionTypes.CONNECTION_PARAMS) &&
        connectionDBState.dbType === 'postgres' ? (
          <>
            <LabeledInput
              label="Host"
              placeholder="localhost"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_HOST',
                  data: e.target.value,
                })
              }
              value={connectionDBState.connectionParamState.host}
              data-test="host"
            />
            <LabeledInput
              label="Port"
              placeholder="5432"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_PORT',
                  data: e.target.value,
                })
              }
              value={connectionDBState.connectionParamState.port}
              data-test="port"
            />
            <LabeledInput
              label="Username"
              placeholder="postgres_user"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_USERNAME',
                  data: e.target.value,
                })
              }
              value={connectionDBState.connectionParamState.username}
              data-test="username"
            />
            <LabeledInput
              label="Password"
              key="connect-db-password"
              type="password"
              placeholder="postgrespassword"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_PASSWORD',
                  data: e.target.value,
                })
              }
              value={connectionDBState.connectionParamState.password}
              data-test="password"
            />
            <LabeledInput
              key="connect-db-database-name"
              label="Database Name"
              placeholder="postgres"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_DATABASE_NAME',
                  data: e.target.value,
                })
              }
              value={connectionDBState.connectionParamState.database}
              data-test="database-name"
            />
          </>
        ) : null}
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
              <div className={styles.connection_settings_form_input_layout}>
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
              <div className={styles.connection_settings_form_input_layout}>
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
              <div className={styles.connection_settings_form_input_layout}>
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
            </div>
          ) : null}
        </div>
      </div>
    </>
  );
};

export default ConnectDatabaseForm;
