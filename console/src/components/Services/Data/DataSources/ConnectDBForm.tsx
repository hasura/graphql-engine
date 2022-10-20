import React, { ChangeEvent, Dispatch } from 'react';
import { FaCheckCircle, FaInfoCircle } from 'react-icons/fa';
import { IconTooltip } from '@/new-components/Tooltip';

import { ConnectDBActions, ConnectDBState, connectionTypes } from './state';
import { LabeledInput } from '../../../Common/LabeledInput';
import { Driver, getSupportedDrivers } from '../../../../dataSources';

import styles from './DataSources.module.scss';
import JSONEditor from '../TablePermissions/JSONEditor';
import { SupportedFeaturesType } from '../../../../dataSources/types';
import { Path } from '../../../Common/utils/tsUtils';
import ConnectionSettingsForm from './ConnectionSettings/ConnectionSettingsForm';
import { GraphQLFieldCustomizationContainer } from './GraphQLFieldCustomization/GraphQLFieldCustomizationContainer';
import { SampleDBTrial } from './SampleDatabase';

export interface ConnectDatabaseFormProps {
  // Connect DB State Props
  connectionDBState: ConnectDBState;
  sampleDBTrial?: SampleDBTrial;
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
  // Connection Type Props - for the Radio buttons
  updateConnectionTypeRadio: (e: ChangeEvent<HTMLInputElement>) => void;
  changeConnectionType?: (value: string) => void;
  connectionTypeState: string;
  // Other Props
  isReadReplica?: boolean;
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
  cockroach: 'postgresql://username:password@hostname:5432/database',
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
    info: 'Only Database URLs and Environment Variables are available for MS SQL Server',
  },
  bigquery: {
    label: 'BigQuery',
    defaultConnection: 'CONNECTION_PARAMETERS',
    info: 'Only Connection Parameters and Environment Variables are available for BigQuery',
  },
  citus: {
    label: 'Citus',
    defaultConnection: 'DATABASE_URL',
  },
  cockroach: {
    label: 'CockroachDB',
    defaultConnection: 'DATABASE_URL',
  },
};

const ConnectDatabaseForm = (props: ConnectDatabaseFormProps) => {
  const {
    connectionDBState,
    connectionDBStateDispatch,
    updateConnectionTypeRadio,
    connectionTypeState,
    isReadReplica = false,
    title,
    isEditState,
  } = props;

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

  return (
    <>
      <p
        className={`${styles.remove_pad_bottom} mb-md flex items-center gap-1 ${styles.connect_db_header}`}
      >
        {title ?? defaultTitle}
        <IconTooltip message="Environment variable recommended" />
        <a
          href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database"
          target="_blank"
          rel="noopener noreferrer"
        >
          <span className={`${styles.fontStyleItalic} ${styles.knowMoreLink}`}>
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
                !isReadReplica
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
            <FaCheckCircle
              className={`${styles.color_green} ${styles.padd_small_right}`}
            />
            <span className={styles.text_muted}>
              Environment variable recommended
            </span>
          </div>
        ) : null}
        {driverToLabel[connectionDBState.dbType].info ? (
          <div>
            <FaInfoCircle className={`${styles.padd_small_right}`} />
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
              <div
                className={`${styles.add_mar_bottom_mid} flex items-center gap-1`}
              >
                <b>Service Account Key:</b>
                <IconTooltip message="Service account key for BigQuery data source" />
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
      {getSupportedDrivers('connectDbForm.extensions_schema').includes(
        connectionDBState.dbType
      ) ? (
        <LabeledInput
          label="Extensions Schema"
          onChange={e => {
            const data =
              e.target?.value?.length > 1 ? e.target.value : undefined;
            connectionDBStateDispatch({
              type: 'UPDATE_EXTENSIONS_SCHEMA',
              data,
            });
          }}
          type="text"
          value={connectionDBState.extensionsSchema}
          placeholder="public"
          tooltipText="Name of the schema where the graphql-engine will install database extensions (default: `public`). Specified schema should be present in the search path of the database."
          data-test="extensions_schema"
        />
      ) : null}

      <ConnectionSettingsForm
        connectionDBState={connectionDBState}
        connectionDBStateDispatch={connectionDBStateDispatch}
        isEditState={isEditState}
      />
      {/*
        TODO: remove the edit state condition when the BE issue is solved
        https://github.com/hasura/graphql-engine-mono/issues/4700
      */}

      <GraphQLFieldCustomizationContainer
        rootFields={connectionDBState.customization?.rootFields}
        typeNames={connectionDBState.customization?.typeNames}
        namingConvention={connectionDBState.customization?.namingConvention}
        connectionDBStateDispatch={connectionDBStateDispatch}
        connectionDBState={connectionDBState}
      />
    </>
  );
};

export default ConnectDatabaseForm;
