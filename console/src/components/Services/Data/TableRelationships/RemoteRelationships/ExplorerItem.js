import React from 'react';
import styles from './SchemaExplorer.scss';
import { columnScalar as columnScalarTooltip } from './Tooltips';
import ValueInput from './ValueInput';

const ExplorerItem = ({
  label,
  checkboxStyle,
  columns,
  handleArgValueChange,
  toggle,
  item,
}) => {
  const {
    isArg,
    isScalar,
    isChecked,
    parentFieldName,
    parentFieldNesting,
  } = item;

  const isLeaf = isArg && isChecked && isScalar;

  const tooltip =
    isLeaf && columnScalarTooltip(`${item.parentArg}.${item.name}`);

  const valueInput = () => {
    if (!isLeaf) {
      return;
    }

    const onValueChange = (value, isColumn) => {
      handleArgValueChange(
        value,
        isColumn,
        parentFieldName,
        parentFieldNesting,
        item
      );
    };

    return (
      <ValueInput
        selectOptions={columns}
        onValueChange={onValueChange}
        staticValue={item.static}
        columnValue={item.column}
        type={item.static ? 'static' : 'column'}
      />
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
        {isArg && isChecked && isScalar && ':'}
      </div>
      <div className={styles.add_mar_right_small}>{valueInput()}</div>
      <div>{tooltip}</div>
    </div>
  );
};

export default ExplorerItem;
