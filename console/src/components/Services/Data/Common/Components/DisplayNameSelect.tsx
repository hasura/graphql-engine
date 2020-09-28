import React from 'react';
import styles from '../../../../Common/TableCommon/Table.scss';

type OptionsType = {
  name: string;
  type: string; // TODO replace with common pg type while Type Imports
};
export interface DisplayNameSelectProps {
  displayName?: string;
  options: OptionsType[];
  onChange: (val: string) => void;
}

export const DisplayNameSelect: React.FC<DisplayNameSelectProps> = ({
  displayName = '',
  options = [{ name: 'test', type: 'text' }],
  onChange = console.warn,
}) => {
  const onChangeEvent = (e: React.ChangeEvent<HTMLSelectElement>) => {
    onChange(e.target.value);
  };
  return (
    <select
      className={`form-control ${styles.select} ${styles.wd100Percent}`}
      value={displayName}
      onChange={onChangeEvent}
      title="Select display column"
      data-test="displayname-select"
    >
      {!displayName && (
        <option value="" disabled>
          -- display_name --
        </option>
      )}
      {displayName && <option value="">-- remove --</option>}
      {options &&
        options.map(dnOpt => {
          return (
            <option key={dnOpt.name} value={dnOpt.name}>
              {dnOpt.name}
            </option>
          );
        })}
    </select>
  );
};
