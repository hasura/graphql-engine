import React, { useState } from 'react';
import styles from './SchemaExplorer.scss';

const ValueInput = ({
  onValueChange,
  columnValue,
  staticValue,
  type,
  selectOptions,
}) => {
  const [isStatic, setIsStatic] = useState(type === 'static');

  const handleColumnSelectChange = e => {
    onValueChange(e.target.value, true);
  };

  const handleInputChange = e => {
    onValueChange(e.target.value, false);
  };

  const handleTypeSelectChange = e => {
    if (!e.target.value) return;
    onValueChange('', e.target.value === 'arg-value-column', false);
    setIsStatic(e.target.value === 'arg-value-static');
  };

  const columnSelects = () => {
    if (isStatic) return null;
    return (
      <select
        value={columnValue || ''}
        className={`form-control ${styles.argValue}`}
        onChange={handleColumnSelectChange}
      >
        {!isStatic && !columnValue && (
          <option key="arg-value-col-placeholder" value="">
            -- column-name --
          </option>
        )}
        {selectOptions.map(o => {
          return (
            <option key={o} value={o}>
              {o}
            </option>
          );
        })}
      </select>
    );
  };

  const argValueTextBox = () => {
    if (!isStatic) return null;
    return (
      <input
        type="text"
        value={staticValue || ''}
        placeholder="Value"
        className={styles.argValue}
        onChange={handleInputChange}
      />
    );
  };

  return (
    <div className={styles.display_flex}>
      <select
        onChange={handleTypeSelectChange}
        value={isStatic ? 'arg-value-static' : 'arg-value-column'}
        className={`form-control ${styles.argValue}`}
      >
        <option key="arg-value-column" value="arg-value-column">
          From Column
        </option>
        <option key="arg-value-static" value="arg-value-static">
          From Static Value
        </option>
      </select>
      {columnSelects()}
      {argValueTextBox()}
    </div>
  );
};

export default ValueInput;
