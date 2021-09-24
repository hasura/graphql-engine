import React from 'react';

import { dataSource } from '../../../../../dataSources';
import styles from '../../../../Common/TableCommon/Table.scss';
import { TypedInput } from './TypedInput';
import { TableColumn } from '../../../../../dataSources/types';

const getColumnInfo = (
  col: TableColumn,
  prevValue?: unknown,
  clone?: Record<string, unknown>
) => {
  const isEditing = prevValue !== undefined;

  const hasDefault = col.column_default
    ? col.column_default.trim() !== ''
    : false;

  const isAutoIncrement = dataSource.isColumnAutoIncrement(col);
  const isIdentity = col.is_identity;
  const isGenerated = col.is_generated;
  const isNullable = col.is_nullable ? col.is_nullable !== 'NO' : false;
  const identityGeneration = col.identity_generation;

  const isDisabled = isAutoIncrement || isGenerated || isIdentity;

  let columnValueType: 'default' | 'null' | 'value' | '';
  switch (true) {
    case isEditing:
      columnValueType = '';
      break;

    case !isEditing && !clone && (isIdentity || hasDefault || isGenerated):
    case clone && isDisabled:
    case identityGeneration === 'ALWAYS':
      columnValueType = 'default';
      break;

    case clone &&
      clone[col.column_name] !== undefined &&
      clone[col.column_name] !== null:
      columnValueType = 'value';
      break;

    case prevValue === null:
    case !prevValue && isNullable:
      columnValueType = 'null';
      break;

    default:
      columnValueType = 'value';
      break;
  }

  return {
    colName: col.column_name,
    isDisabled,
    isNullable,
    hasDefault,
    isIdentity,
    isGenerated,
    identityGeneration,
    columnValueType,
  };
};

export interface TableRowProps {
  column: TableColumn;
  setRef: (
    refName: 'valueNode' | 'nullNode' | 'defaultNode' | 'insertRadioNode',
    node: HTMLInputElement | null
  ) => void;
  enumOptions: Record<string, any>;
  index: number;
  clone?: Record<string, any>;
  onChange?: (e: React.ChangeEvent<HTMLInputElement>, val: unknown) => void;
  onFocus?: (e: React.FocusEvent<HTMLInputElement>) => void;
  prevValue?: unknown;
}

export const TableRow: React.FC<TableRowProps> = ({
  column,
  onChange,
  onFocus,
  setRef,
  enumOptions,
  index,
  clone,
  prevValue,
}) => {
  const {
    colName,
    isDisabled,
    isNullable,
    hasDefault,
    columnValueType,
  } = getColumnInfo(column, prevValue, clone);

  const handleChange = (
    e: React.ChangeEvent<HTMLInputElement>,
    val: string
  ) => {
    if (isDisabled || isNullable || hasDefault) return;

    if (onChange) {
      onChange(e, val);
    }
  };

  const handleFocus = (e: React.FocusEvent<HTMLInputElement>) => {
    if (isDisabled || isNullable || hasDefault) return;

    if (onFocus) {
      onFocus(e);
    }
  };

  return (
    <div className="form-group">
      <label
        className={`col-sm-3 control-label ${styles.insertBoxLabel}`}
        title={colName}
      >
        {colName}
      </label>
      <label className={`${styles.radioLabel} radio-inline`}>
        <input
          type="radio"
          className="legacy-input-fix"
          ref={node => {
            setRef('insertRadioNode', node);
          }}
          name={`${colName}-value`}
          defaultChecked={columnValueType === 'value'}
          disabled={isDisabled}
        />
        <TypedInput
          inputRef={(node: HTMLInputElement) => {
            setRef('valueNode', node);
          }}
          prevValue={prevValue}
          enumOptions={enumOptions}
          col={column}
          clone={clone}
          onChange={handleChange}
          onFocus={handleFocus}
          disabled={isDisabled}
          index={index}
        />
      </label>
      <label className={`${styles.radioLabel} radio-inline`}>
        <input
          type="radio"
          className="legacy-input-fix"
          ref={node => {
            setRef('nullNode', node);
          }}
          disabled={!isNullable}
          defaultChecked={columnValueType === 'null'}
          name={`${colName}-value`}
          data-test={`nullable-radio-${index}`}
        />
        <span className={styles.radioSpan}>NULL</span>
      </label>
      <label className={`${styles.radioLabel} radio-inline`}>
        <input
          type="radio"
          className="legacy-input-fix"
          ref={node => {
            setRef('defaultNode', node);
          }}
          name={`${colName}-value`}
          disabled={!hasDefault}
          defaultChecked={columnValueType === 'default'}
          data-test={`typed-input-default-${index}`}
        />
        <span className={styles.radioSpan}>Default</span>
      </label>
    </div>
  );
};
