import { ConnectionSettingsFormProps } from '../ConnectionSettingsForm';
import { IconTooltip } from '../../../../../../new-components/Tooltip';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const PreparedStatements: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => (
  <div className={`${styles.add_mar_bottom_mid} ${styles.checkbox_margin_top}`}>
    <label className="inline-flex items-center">
      <input
        type="checkbox"
        checked={connectionDBState.preparedStatements}
        className="legacy-input-fix"
        onChange={e => {
          connectionDBStateDispatch({
            type: 'UPDATE_PREPARED_STATEMENTS',
            data: e.target.checked,
          });
        }}
      />{' '}
      &nbsp;
      <b>Use Prepared Statements</b>
      <IconTooltip message="Prepared statements are disabled by default" />
    </label>
  </div>
);
