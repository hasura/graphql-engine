import React, { FormEvent } from 'react';
import { LabeledInput } from '../../../Common/LabeledInput';
import { Connect, useAvailableDrivers } from '../../../../features/ConnectDB';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { Button } from '../../../../new-components/Button';
import ConnectDatabaseForm, { ConnectDatabaseFormProps } from './ConnectDBForm';
import styles from './DataSources.module.scss';
import { Driver } from '../../../../dataSources';
import { isDBSupported } from './utils';

interface DataSourceFormWrapperProps extends ConnectDatabaseFormProps {
  loading: boolean;
  onSubmit: (e: FormEvent<HTMLFormElement>) => void;
}

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
  alloy: {
    label: 'AlloyDB',
    defaultConnection: 'DATABASE_URL',
  },
};

const DataSourceFormWrapper: React.FC<DataSourceFormWrapperProps> = props => {
  const {
    onSubmit,
    loading,
    isEditState,
    children,
    isReadReplica,
    connectionDBState,
    connectionDBStateDispatch,
    changeConnectionType,
    connectionTypeState,
  } = props;

  const { isLoading, data: drivers } = useAvailableDrivers();

  const handleDBChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const value = e.target.value as Driver;
    const isSupported = isDBSupported(value, connectionTypeState);
    connectionDBStateDispatch({
      type: 'UPDATE_DB_DRIVER',
      data: value,
    });

    /**
     * Early return for gdc drivers when feature flag is enabled
     */
    // const driver = drivers?.find(d => d.name === value);
    // if (!driver?.native) return;

    if (!isSupported && changeConnectionType) {
      changeConnectionType(driverToLabel[value].defaultConnection);
    }
  };

  if (isLoading) return <>Loading...</>;

  if (!drivers) return <>Something went wrong</>;

  const nativeDrivers = drivers
    .filter(driver => driver.native)
    .map(driver => driver.name);

  return (
    <>
      {!nativeDrivers.includes(connectionDBState.dbType) ? (
        <Analytics name="EditDataSource" {...REDACT_EVERYTHING}>
          <div className="max-w-xl">
            <Connect.CreateConnection
              name={connectionDBState.displayName}
              driver={connectionDBState.dbType}
              onDriverChange={(driver, name) => {
                connectionDBStateDispatch({
                  type: 'UPDATE_DB_DRIVER',
                  data: driver as any,
                });
                connectionDBStateDispatch({
                  type: 'UPDATE_DISPLAY_NAME',
                  data: name,
                });
              }}
            />
          </div>
        </Analytics>
      ) : (
        <Analytics name="EditDataSource" {...REDACT_EVERYTHING}>
          <form
            onSubmit={onSubmit}
            className={`${styles.connect_db_content} max-w-screen-md`}
          >
            <div className="max-w-xl">
              {!isReadReplica && (
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
                    {(drivers ?? []).map(driver => (
                      <option key={driver.name} value={driver.name}>
                        {driver.displayName}{' '}
                        {driver.release === 'GA' ? null : `(${driver.release})`}
                      </option>
                    ))}
                  </select>
                </>
              )}
            </div>

            <ConnectDatabaseForm isEditState={isEditState} {...props} />
            {children}
            <div>
              <Analytics
                name="data-tab-connect-db-button"
                passHtmlAttributesToChildren
              >
                <Button
                  className="mt-sm"
                  size="lg"
                  mode="primary"
                  type="submit"
                  style={{
                    ...(loading && { cursor: 'progress' }),
                  }}
                  isLoading={loading}
                  loadingText="Saving..."
                  data-test="connect-database-btn"
                >
                  {!isEditState ? 'Connect Database' : 'Update Connection'}
                </Button>
              </Analytics>
            </div>
          </form>
        </Analytics>
      )}
    </>
  );
};

export default DataSourceFormWrapper;
