import { LabeledInput } from '../../../../../Common/LabeledInput';
import { ConnectionSettingsFormProps } from '../ConnectionSettingsForm';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const ConnectionLifetime: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => (
  <div className={styles.connection_settings_input_layout}>
    <LabeledInput
      label="Connection Lifetime"
      tooltipText="Time (in seconds) from connection creation after which the connection should be destroyed and a new one created. A value of 0 indicates we should never destroy an active connection. If 0 is passed, memory from large query results may not be reclaimed."
      type="number"
      className={`form-control ${styles.connnection_settings_form_input}`}
      placeholder="600"
      value={
        connectionDBState.connectionSettings?.connection_lifetime ?? undefined
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
);
