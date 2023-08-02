import { LabeledInput } from '../../../../../Common/LabeledInput';
import { ConnectionSettingsFormProps } from '../ConnectionSettingsForm';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const PoolTimeout: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => (
  <div className={styles.connection_settings_input_layout}>
    <LabeledInput
      label="Pool Timeout"
      tooltipText="Maximum time (in seconds) to wait while acquiring a Postgres connection from the pool"
      type="number"
      className={`form-control ${styles.connnection_settings_form_input}`}
      placeholder="360"
      value={connectionDBState.connectionSettings?.pool_timeout ?? undefined}
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
);
