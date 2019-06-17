import React, { useEffect } from 'react';
import styles from './SchemaExplorer.scss';
import { getSchemaTree } from '../utils';

const CheckboxWithLabel = ({
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

  const columnSelect = () => {
    if (
      !isArg ||
      !isChecked ||
      !(isScalar || isScalarList || isNonNullableScalar)
    ) {
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
        <select defaultValue="" onChange={onColumnChange}>
          <option key="placeholder" value="">
            -- column --
          </option>
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
      onClick={toggle}
      style={checkboxStyle}
    >
      <div className={styles.add_mar_right_small}>
        <input
          checked={isChecked}
          type="checkbox"
          className={styles.cursorPointer}
        />
      </div>
      <div className={styles.add_mar_right_small}>
        {label}
        {isArg &&
          isChecked &&
          (isScalar || isNonNullableScalar || isScalarList) &&
          ':'}
      </div>
      <div>{columnSelect()}</div>
    </div>
  );
};

const SchemaExplorer = ({
  schema,
  relationship,
  handleArgChange,
  handleColumnChange,
  handleRemoteFieldChange,
  tableSchema,
}) => {
  useEffect(() => {}, []);

  if (!relationship.remoteSchema) {
    return <i>No schema selected</i>;
  }

  const schemaTree = getSchemaTree(
    relationship,
    Object.values(schema._queryType._fields)
  );

  const columns = tableSchema.columns.map(c => c.column_name).sort();

  return (
    <div className={styles.schemaExplorerContainer}>
      {schemaTree.map(f => {
        const checkboxStyle = {
          marginLeft: `${(f.nesting + (f.argNesting || 0)) * 20}px`,
          color: f.isArg ? '#8B2BB9' : 'rgb(31, 97, 160)',
          fontStyle: f.isArg ? 'italic' : 'normal',
          cursor: 'pointer',
        };

        const toggle = () => {
          const checked = !f.isChecked;
          if (f.isArg) {
            handleArgChange(
              f.parentFieldName,
              f.parentFieldNesting,
              f.name,
              f.argNesting,
              checked,
              f.parentArg
            );
          } else {
            handleRemoteFieldChange(f.name, f.nesting, checked);
          }
        };

        return (
          <CheckboxWithLabel
            label={f.name}
            checkboxStyle={checkboxStyle}
            toggle={toggle}
            item={f}
            columns={columns}
            handleColumnChange={handleColumnChange}
          />
        );
      })}
    </div>
  );
};

export default SchemaExplorer;
