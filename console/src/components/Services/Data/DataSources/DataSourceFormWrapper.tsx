import React, { FormEvent } from 'react';
import { LabeledInput } from '@/components/Common/LabeledInput';
import { Connect, useAvailableDrivers } from '@/features/ConnectDB';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '@/features/FeatureFlags';
import { Button } from '@/new-components/Button';
import ConnectDatabaseForm, { ConnectDatabaseFormProps } from './ConnectDBForm';
import styles from './DataSources.module.scss';
import { SampleDBSection } from './SampleDatabase';
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
};

const DataSourceFormWrapper: React.FC<DataSourceFormWrapperProps> = props => {
  const {
    onSubmit,
    loading,
    isEditState,
    children,
    sampleDBTrial,
    isReadReplica,
    connectionDBState,
    connectionDBStateDispatch,
    changeConnectionType,
    connectionTypeState,
  } = props;

  const { isLoading, data: drivers } = useAvailableDrivers();

  const { enabled: isGDCFeatureFlagEnabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.gdcId
  );

  const onSampleDBTry = () => {
    if (!sampleDBTrial || !sampleDBTrial.isActive()) return;

    sampleDBTrial.track.tryButton();

    if (!sampleDBTrial.isActive()) return;

    connectionDBStateDispatch({
      type: 'UPDATE_DISPLAY_NAME',
      data: 'SampleDB',
    });
    connectionDBStateDispatch({
      type: 'UPDATE_DB_DRIVER',
      data: 'postgres',
    });
    connectionDBStateDispatch({
      type: 'UPDATE_DB_URL',
      data: sampleDBTrial.getDatabaseUrl(),
    });
  };

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
    const driver = drivers?.find(d => d.name === value);
    if (isGDCFeatureFlagEnabled && !driver?.native) return;

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
      {isGDCFeatureFlagEnabled &&
      !nativeDrivers.includes(connectionDBState.dbType) ? (
        <div className="max-w-xl">
          <Connect
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
      ) : (
        <form
          onSubmit={onSubmit}
          className={`${styles.connect_db_content} ${styles.connect_form_width}`}
        >
          <div className="max-w-xl">
            {!isReadReplica && (
              <>
                {sampleDBTrial && sampleDBTrial.isActive() && (
                  <div className="mb-md">
                    <SampleDBSection onTrySampleDB={onSampleDBTry} />
                  </div>
                )}
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
                  {(drivers ?? [])
                    /**
                     * Why this filter? if GDC feature flag is not enabled, then I want to see only native sources
                     */
                    .filter(driver => driver.native || isGDCFeatureFlagEnabled)
                    .map(driver => (
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
          <div className={styles.add_button_layout}>
            <Button
              size="lg"
              mode="primary"
              type="submit"
              style={{
                ...(loading && { cursor: 'progress' }),
              }}
              isLoading={loading}
              loadingText="Saving..."
              data-test="connect-database-btn"
              data-trackid="data-tab-connect-db-button"
            >
              {!isEditState ? 'Connect Database' : 'Update Connection'}
            </Button>
          </div>
        </form>
      )}
    </>
  );
};

export default DataSourceFormWrapper;
