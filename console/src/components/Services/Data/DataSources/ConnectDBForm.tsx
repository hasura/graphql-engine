import React, { ChangeEvent, Dispatch } from 'react';

import { ConnectDBActions, ConnectDBState, connectionTypes } from './state';
import { LabeledInput } from '../../../Common/LabeledInput';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { Driver, getSupportedDrivers } from '../../../../dataSources';

import styles from './DataSources.scss';
import JSONEditor from '../TablePermissions/JSONEditor';
import { SupportedFeaturesType } from '../../../../dataSources/types';
import { Path } from '../../../Common/utils/tsUtils';
import ConnectionSettingsForm from './ConnectionSettingsForm';

export interface ConnectDatabaseFormProps {
  // Connect DB State Props
  connectionDBState: ConnectDBState;
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
  // Connection Type Props - for the Radio buttons
  updateConnectionTypeRadio: (e: ChangeEvent<HTMLInputElement>) => void;
  changeConnectionType?: (value: string) => void;
  connectionTypeState: string;
  // Other Props
  isreadreplica?: boolean;
  isEditState?: boolean;
  title?: string;
}

export const connectionRadios = [
  {
    value: connectionTypes.ENV_VAR,
    title: 'Environment Variable',
  },
  {
    value: connectionTypes.DATABASE_URL,
    title: 'Database URL',
  },
  {
    value: connectionTypes.CONNECTION_PARAMS,
    title: 'Connection Parameters',
  },
];

const dbTypePlaceholders: Record<Driver, string> = {
  postgres: 'postgresql://username:password@hostname:5432/database',
  citus: 'postgresql://username:password@hostname:5432/database',
  mssql:
    'Driver={ODBC Driver 17 for SQL Server};Server=serveraddress;Database=dbname;Uid=username;Pwd=password;',
  mysql: 'MySQL connection string',
  bigquery: 'SERVICE_ACCOUNT_KEY_FROM_ENV',
};

const defaultTitle = 'Connect Database Via';

const driverToLabel: Record<
  Driver,
  { label: string; defaultConnection: string; info?: string; beta?: boolean }
> = {
  mysql: { label: 'MySQL', defaultConnection: 'DATABASE_URL' },
  postgres: { label: 'PostgreSQL', defaultConnection: 'DATABASE_URL' },
  mssql: {
    label: 'MS SQL Server',
    defaultConnection: 'DATABASE_URL',
    info:
      'Only Database URLs and Environment Variables are available for MS SQL Server',
  },
  bigquery: {
    label: 'BigQuery',
    defaultConnection: 'CONNECTION_PARAMETERS',
    info:
      'Only Connection Parameters and Environment Variables are available for BigQuery',
    beta: true,
  },
  citus: {
    label: 'Citus',
    defaultConnection: 'DATABASE_URL',
  },
};

const supportedDrivers = getSupportedDrivers('connectDbForm.enabled');

const ConnectDatabaseForm: React.FC<ConnectDatabaseFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
  changeConnectionType,
  updateConnectionTypeRadio,
  connectionTypeState,
  isreadreplica = false,
  isEditState = false,
  title,
}) => {
  const isDBSupported = (driver: Driver, connectionType: string) => {
    let ts = 'databaseURL';
    if (connectionType === 'CONNECTION_PARAMETERS') {
      ts = 'connectionParameters';
    }
    if (connectionType === 'ENVIRONMENT_VARIABLES') {
      ts = 'environmentVariable';
    }
    return getSupportedDrivers(
      `connectDbForm.${ts}` as Path<SupportedFeaturesType>
    ).includes(driver);
  };

  const handleDBChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const value = e.target.value as Driver;
    const isSupported = isDBSupported(value, connectionTypeState);
    connectionDBStateDispatch({
      type: 'UPDATE_DB_DRIVER',
      data: value,
    });
    if (!isSupported && changeConnectionType) {
      changeConnectionType(driverToLabel[value].defaultConnection);
    }
  };

  return (
    <>
      <div className={styles.connect_form_layout}>
        {!isreadreplica ? (
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
              onChange={handleDBChange}
              className={`form-control ${styles.connect_db_input_pad}`}
              disabled={isEditState}
              data-test="database-type"
            >
              {supportedDrivers.map(driver => (
                <option key={driver} value={driver}>
                  {driverToLabel[driver].label}
                  {driverToLabel[driver].beta && ' (beta)'}
                </option>
              ))}
            </select>
          </>
        ) : null}
        <p
          className={`${styles.remove_pad_bottom} mb-md ${styles.connect_db_header}`}
        >
          {title ?? defaultTitle}
          <Tooltip message="Environment variable recommended" />
          <a
            href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database"
            target="_blank"
            rel="noopener noreferrer"
          >
            <span
              className={`${styles.fontStyleItalic} ${styles.knowMoreLink}`}
            >
              (Know More)
            </span>
          </a>
        </p>
        <div
          className={styles.connect_db_radios}
          onChange={updateConnectionTypeRadio}
        >
          {connectionRadios.map(radioBtn => (
            <label
              key={`label-${radioBtn.title}`}
              className={`${styles.connect_db_radio_label} inline-flex ${
                !isDBSupported(connectionDBState.dbType, radioBtn.value)
                  ? styles.label_disabled
                  : ''
              }`}
            >
              <input
                type="radio"
                value={radioBtn.value}
                className="legacy-input-fix"
                name={
                  !isreadreplica
                    ? 'connection-type'
                    : 'connection-type-read-replica'
                }
                checked={connectionTypeState.includes(radioBtn.value)}
                defaultChecked={
                  connectionTypeState === connectionTypes.DATABASE_URL
                }
                disabled={
                  !isDBSupported(connectionDBState.dbType, radioBtn.value)
                }
              />
              {radioBtn.title}
            </label>
          ))}
        </div>
        <div className={styles.add_mar_bottom_mid}>
          {connectionTypeState.includes(connectionTypes.ENV_VAR) ? (
            <div className={styles.add_mar_bottom_mid}>
              <i
                className={`fa fa-check-circle ${styles.color_green} ${styles.padd_small_right}`}
              />
              <span className={styles.text_muted}>
                Environment variable recommended
              </span>
            </div>
          ) : null}
          {driverToLabel[connectionDBState.dbType].info ? (
            <div>
              <i className={`fa fa-info-circle ${styles.padd_small_right}`} />
              <span className={styles.text_muted}>
                {driverToLabel[connectionDBState.dbType].info}
              </span>
            </div>
          ) : null}
        </div>
        {connectionTypeState.includes(connectionTypes.DATABASE_URL) ||
        (connectionTypeState.includes(connectionTypes.CONNECTION_PARAMS) &&
          connectionDBState.dbType === 'mssql') ? (
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
                  <b>Service Account Key:</b>
                  <Tooltip message="Service account key for BigQuery data source" />
                </div>
                <JSONEditor
                  minLines={5}
                  initData="{}"
                  onChange={value => {
                    connectionDBStateDispatch({
                      type: 'UPDATE_DB_BIGQUERY_SERVICE_ACCOUNT',
                      data: value,
                    });
                  }}
                  data={connectionDBState.databaseURLState.serviceAccount}
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
            <LabeledInput
              label="Global Select Limit"
              onChange={e => {
                let data = Number.parseInt(e.target.value, 10);
                if (Number.isNaN(data) || data <= 0) {
                  data = 0;
                }
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_BIGQUERY_GLOBAL_LIMIT',
                  data,
                });
              }}
              type="number"
              min="0"
              value={connectionDBState.databaseURLState.global_select_limit}
              placeholder="1000"
              data-test="global_select_limit"
            />
          </>
        ) : null}
        {connectionTypeState.includes(connectionTypes.CONNECTION_PARAMS) &&
          getSupportedDrivers('connectDbForm.connectionParameters').includes(
            connectionDBState.dbType
          ) &&
          connectionDBState.dbType !== 'bigquery' && (
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
          )}
        <ConnectionSettingsForm
          connectionDBState={connectionDBState}
          connectionDBStateDispatch={connectionDBStateDispatch}
        />
      </div>
      <hr className={styles.line_width} />
    </>
  );
};

export default ConnectDatabaseForm;
