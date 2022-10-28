import React from 'react';
import { LabeledInput } from '@/components/Common/LabeledInput';
import { ConnectionSettingsFormProps } from '@/components/Services/Data/DataSources/ConnectionSettings/ConnectionSettingsForm';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { FaExclamationTriangle } from 'react-icons/fa';
import styles from '../../DataSources.module.scss';

export const CumulativeMaxConnections: React.VFC<ConnectionSettingsFormProps> =
  ({ connectionDBState, connectionDBStateDispatch }) => (
    <>
      <div className={styles.connection_settings_input_layout}>
        <LabeledInput
          label="Total Max Connections"
          tooltipText="Maximum number of database connections"
          type="number"
          className={`form-control ${styles.connnection_settings_form_input}`}
          placeholder="50"
          value={
            connectionDBState.connectionSettings?.total_max_connections ||
            undefined
          }
          onChange={e =>
            connectionDBStateDispatch({
              type: 'UPDATE_TOTAL_MAX_CONNECTIONS',
              data: e.target.value,
            })
          }
          min="0"
          boldlabel
          data-test="max-connections"
          icon={
            connectionDBState?.connectionSettings?.max_connections !== undefined
          }
        />
      </div>
      {connectionDBState?.connectionSettings?.max_connections && (
        <IndicatorCard status="info">
          <p className="font-bold">
            <FaExclamationTriangle className="text-blue-300 pb-1 mr-4 text-lg" />
            Set Total Max Connections
          </p>
          <p className="ml-lg">
            You have set <b>Max Connections Per Instance</b> which is not
            recommended for Hasura Cloud. Use <b>Total Max Connections</b>{' '}
            instead.
          </p>
        </IndicatorCard>
      )}
    </>
  );
