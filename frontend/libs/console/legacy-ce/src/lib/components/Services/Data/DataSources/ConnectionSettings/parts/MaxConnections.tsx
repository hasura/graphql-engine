import { LabeledInput } from '@/components/Common/LabeledInput';
import { ConnectionSettingsFormProps } from '@/components/Services/Data/DataSources/ConnectionSettings/ConnectionSettingsForm';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const MaxConnections: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => (
  <div className={styles.connection_settings_input_layout}>
    <LabeledInput
      label="Max Connections Per Instance"
      tooltipText="Maximum number of database connections per instance"
      type="number"
      className={`form-control ${styles.connnection_settings_form_input}`}
      placeholder="50"
      value={connectionDBState.connectionSettings?.max_connections ?? undefined}
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
);
