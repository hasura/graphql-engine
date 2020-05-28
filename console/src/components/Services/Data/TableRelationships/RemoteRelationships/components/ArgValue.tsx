import React from 'react';
import { ArgValue } from '../utils';
import styles from '../SchemaExplorer.scss';

type Props = {
  value: ArgValue;
  handleArgValueKindChange: (e: React.ChangeEvent<HTMLSelectElement>) => void;
  handleArgValueChange: (e: React.BaseSyntheticEvent) => void;
  columns: string[];
};

const ArgValueElement: React.FC<Props> = ({
  columns,
  value,
  handleArgValueChange,
  handleArgValueKindChange,
}) => {
  return (
    <div className={styles.display_flex}>
      :&nbsp;
      <select
        onChange={handleArgValueKindChange}
        value={value.kind}
        className={`form-control ${styles.argValue}`}
      >
        <option key="arg-value-column" value="column">
          From Column
        </option>
        <option key="arg-value-static" value="static">
          From Static Value
        </option>
      </select>
      {value.kind === 'column' ? (
        <select
          value={value.value}
          className={`form-control ${styles.argValue}`}
          onChange={handleArgValueChange}
        >
          {!value.value && (
            <option key="arg-value-col-placeholder" value="">
              -- column-name --
            </option>
          )}
          {columns.map(o => {
            return (
              <option key={o} value={o}>
                {o}
              </option>
            );
          })}
        </select>
      ) : (
        <input
          type="text"
          value={value.value}
          placeholder="Value"
          className={`form-control ${styles.argValue}`}
          onChange={handleArgValueChange}
        />
      )}
    </div>
  );
};

export default ArgValueElement;
