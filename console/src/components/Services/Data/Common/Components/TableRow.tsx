import React from 'react';

import { isColumnAutoIncrement } from '../../../../Common/utils/pgUtils';
import styles from '../../../../Common/TableCommon/Table.scss';
import { TypedInput } from './TypedInput';

type Column = {
  column_name?: string;
  is_generated?: string;
  is_nullable?: string;
  is_identity?: string;
  column_default?: string;
};

export interface TableRowProps {
  column: Column;
  setRef: (
    refName: 'valueNode' | 'nullNode' | 'defaultNode' | 'insertRadioNode',
    node: HTMLInputElement | null
  ) => void;
  enumOptions: object;
  index: number;
  clone?: object;
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
    column_name: colName,
    is_generated,
    is_nullable,
    is_identity,
    column_default,
  } = column;
  const hasDefault = column_default ? column_default.trim() !== '' : false;
  const isNullable = is_nullable ? is_nullable !== 'NO' : false;
  const isIdentity = is_identity ? is_identity !== 'NO' : false;
  const isAutoIncrement = isColumnAutoIncrement(column);
  const isGenerated = is_generated ? is_generated !== 'NO' : false;
  const isDisabled = isAutoIncrement || isGenerated;

  const handleChange = (
    e: React.ChangeEvent<HTMLInputElement>,
    val: string
  ) => {
    if (isDisabled) return;
    if (!isNullable && !hasDefault) return;

    if (onChange) {
      onChange(e, val);
    }
  };

  const handleFocus = (e: React.FocusEvent<HTMLInputElement>) => {
    if (isDisabled) return;
    if (!isNullable && !hasDefault) return;

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
          ref={node => {
            setRef('insertRadioNode', node);
          }}
          name={`${colName}-value`}
          defaultChecked={!!prevValue || (!hasDefault && !isNullable)}
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
          ref={node => {
            setRef('nullNode', node);
          }}
          disabled={!isNullable || isGenerated}
          defaultChecked={prevValue === null || isNullable}
          name={`${colName}-value`}
          data-test={`nullable-radio-${index}`}
        />
        <span className={styles.radioSpan}>NULL</span>
      </label>
      <label className={`${styles.radioLabel} radio-inline`}>
        <input
          type="radio"
          ref={node => {
            setRef('defaultNode', node);
          }}
          name={`${colName}-value`}
          disabled={isDisabled || (!hasDefault && !isIdentity)}
          defaultChecked={
            prevValue !== undefined
              ? false
              : isGenerated || isIdentity || hasDefault
          }
          data-test={`typed-input-default-${index}`}
        />
        <span className={styles.radioSpan}>Default</span>
      </label>
    </div>
  );
};
