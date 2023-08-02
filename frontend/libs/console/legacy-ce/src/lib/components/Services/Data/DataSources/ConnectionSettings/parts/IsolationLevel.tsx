import { ConnectionSettingsFormProps } from '../ConnectionSettingsForm';
import { IsolationLevelOptions } from '../../../../../../metadata/types';
import { IconTooltip } from '../../../../../../new-components/Tooltip';
import React from 'react';
import styles from '../../DataSources.module.scss';

const ISOLATION_LEVEL_OPTIONS: readonly string[] = Object.freeze([
  'read-committed',
  'repeatable-read',
  'serializable',
]);

const isIsolationLevelOption = (
  value: string
): value is IsolationLevelOptions => {
  return ['read-committed', 'repeatable-read', 'serializable'].includes(value);
};

export const IsolationLevel: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => (
  <div className={styles.connection_settings_input_layout}>
    <label className="flex items-center gap-1">
      <b>Isolation Level</b>
      <IconTooltip message="The transaction isolation level in which the queries made to the source will be run" />
    </label>
    <select
      className={`form-control ${styles.connnection_settings_form_input} cursor-pointer`}
      onChange={e => {
        // any way to do this?
        if (isIsolationLevelOption(e.target.value)) {
          connectionDBStateDispatch({
            type: 'UPDATE_ISOLATION_LEVEL',
            data: e.target.value,
          });
        }
      }}
      value={connectionDBState.isolationLevel}
    >
      {ISOLATION_LEVEL_OPTIONS.map(o => (
        <option key={o} value={o}>
          {o}
        </option>
      ))}
    </select>
  </div>
);
