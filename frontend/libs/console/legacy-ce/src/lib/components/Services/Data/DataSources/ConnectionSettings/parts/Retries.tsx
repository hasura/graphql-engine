import { LabeledInput } from '../../../../../Common/LabeledInput';
import { ConnectionSettingsFormProps } from '../ConnectionSettingsForm';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const Retries: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => (
  <div className={styles.connection_settings_input_layout}>
    <LabeledInput
      label="Retries"
      tooltipText="Number of retries to perform"
      type="number"
      className={`form-control ${styles.connnection_settings_form_input}`}
      placeholder="1"
      value={connectionDBState.connectionSettings?.retries ?? undefined}
      onChange={e =>
        connectionDBStateDispatch({
          type: 'UPDATE_RETRIES',
          data: e.target.value,
        })
      }
      min="0"
      boldlabel
      data-test="retries"
    />
  </div>
);
