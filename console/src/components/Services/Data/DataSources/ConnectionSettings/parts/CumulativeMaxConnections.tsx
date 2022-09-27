import { LabeledInput } from '@/components/Common/LabeledInput';
import { ConnectionSettingsFormProps } from '@/components/Services/Data/DataSources/ConnectionSettings/ConnectionSettingsForm';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const CumulativeMaxConnections: React.VFC<ConnectionSettingsFormProps> =
  ({ connectionDBState, connectionDBStateDispatch }) => (
    <div className={styles.connection_settings_input_layout}>
      <LabeledInput
        label="Cumulative Max Connections"
        tooltipText="Maximum number of database connections"
        type="number"
        className={`form-control ${styles.connnection_settings_form_input}`}
        placeholder="50"
        value={
          connectionDBState.connectionSettings?.cumulative_max_connections ||
          undefined
        }
        onChange={e =>
          connectionDBStateDispatch({
            type: 'UPDATE_CUMULATIVE_MAX_CONNECTIONS',
            data: e.target.value,
          })
        }
        min="0"
        boldlabel
        data-test="max-connections"
      />
    </div>
  );
