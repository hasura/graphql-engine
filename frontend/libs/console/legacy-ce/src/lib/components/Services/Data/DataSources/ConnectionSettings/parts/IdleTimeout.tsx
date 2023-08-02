import { LabeledInput } from '../../../../../Common/LabeledInput';
import { ConnectionSettingsFormProps } from '../ConnectionSettingsForm';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const IdleTimeout: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => (
  <div className={styles.connection_settings_input_layout}>
    <LabeledInput
      label="Idle Timeout"
      tooltipText="The idle timeout (in seconds) per connection"
      type="number"
      className={`form-control ${styles.connnection_settings_form_input}`}
      placeholder="180"
      value={connectionDBState.connectionSettings?.idle_timeout ?? undefined}
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
);
