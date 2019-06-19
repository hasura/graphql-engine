import React from 'react';
import styles from './SchemaExplorer.scss';
import { columnScalar as columnScalarTooltip } from './Tooltips';

const ExplorerItem = ({
  label,
  checkboxStyle,
  columns,
  handleColumnChange,
  toggle,
  item,
}) => {
  const {
    isArg,
    isScalar,
    isScalarList,
    isNonNullableScalar,
    isChecked,
    parentFieldName,
    parentFieldNesting,
  } = item;

  const isLeaf =
    isArg && isChecked && (isScalar || isScalarList || isNonNullableScalar);

  const tooltip =
    isLeaf && columnScalarTooltip(`${item.parentArg}.${item.name}`);

  const columnSelect = () => {
    if (!isLeaf) {
      return;
    }
    const onColumnChange = e => {
      if (!e.target.value) return;
      const columnValue = isScalarList ? [e.target.value] : e.target.value;
      handleColumnChange(
        columnValue,
        parentFieldName,
        parentFieldNesting,
        item
      );
    };

    return (
      <div>
        <select
          value={item.column || ''}
          onChange={onColumnChange}
          className={styles.scalarColumnSelect}
        >
          {!item.column && (
            <option key="placeholder" value="">
              -- column --
            </option>
          )}
          {columns.map(c => {
            return (
              <option key={c} value={c}>
                {c}
              </option>
            );
          })}
        </select>
      </div>
    );
  };
  return (
    <div
      className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}
      style={checkboxStyle}
    >
      <div className={styles.add_mar_right_small}>
        <input
          checked={isChecked}
          type="checkbox"
          className={styles.cursorPointer}
          onChange={toggle}
        />
      </div>
      <div
        className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
        onClick={toggle}
      >
        {label}
        {isArg &&
          isChecked &&
          (isScalar || isNonNullableScalar || isScalarList) &&
          ':'}
      </div>
      <div className={styles.add_mar_right_small}>{columnSelect()}</div>
      <div>{tooltip}</div>
    </div>
  );
};

export default ExplorerItem;
