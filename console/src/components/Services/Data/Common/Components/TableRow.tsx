import React from 'react';

import { dataSource } from '../../../../../dataSources';
import { TypedInput } from './TypedInput';
import { TableColumn } from '../../../../../dataSources/types';
import { focusYellowRing } from '../../constants';

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
    refName: 'valueNode' | 'nullNode' | 'defaultNode' | 'radioNode',
    node: HTMLInputElement | null
  ) => void;
  enumOptions: Record<string, any>;
  index: string;
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
  const { colName, isDisabled, isNullable, hasDefault, columnValueType } =
    getColumnInfo(column, prevValue, clone);

  const handleChange = (
    e: React.ChangeEvent<HTMLInputElement>,
    val: string
  ) => {
    if (isDisabled) return;

    if (onChange) {
      onChange(e, val);
    }
  };

  const handleFocus = (e: React.FocusEvent<HTMLInputElement>) => {
    if (isDisabled) return;

    if (onFocus) {
      onFocus(e);
    }
  };

  return (
    <div className="items-center flex pb-xs">
      <div className="w-2/12 overflow-hidden text-ellipsis" title={colName}>
        {colName}
      </div>
      <div>
        <input
          type="radio"
          className={`${focusYellowRing} !m-0 !mr-sm `}
          ref={node => {
            setRef('radioNode', node);
          }}
          name={`${colName}-value`}
          defaultChecked={columnValueType === 'value'}
          disabled={isDisabled}
        />
      </div>
      <div className="flex items-center mr-sm relative">
        <TypedInput
          inputRef={(node: HTMLInputElement) => {
            setRef('valueNode', node);
          }}
          // TODO: #3053 console: fix type coercion to null
          prevValue={prevValue as null}
          enumOptions={enumOptions}
          col={column}
          clone={clone}
          onChange={handleChange}
          onFocus={handleFocus}
          disabled={isDisabled}
          index={index}
        />
      </div>
      <div className="flex items-center mr-sm relative">
        <input
          type="radio"
          className={`${focusYellowRing} !m-0 !mr-sm`}
          ref={node => {
            setRef('nullNode', node);
          }}
          disabled={!isNullable}
          defaultChecked={columnValueType === 'null'}
          name={`${colName}-value`}
          data-test={`null-value-radio-${index}`}
        />
        <div>NULL</div>
      </div>
      <div className="flex items-center mr-sm relative">
        <input
          type="radio"
          className={`${focusYellowRing} !m-0 !mr-sm`}
          ref={node => {
            setRef('defaultNode', node);
          }}
          name={`${colName}-value`}
          disabled={!hasDefault}
          defaultChecked={columnValueType === 'default'}
          data-test={`typed-input-default-${index}`}
        />
        <div>Default</div>
      </div>
    </div>
  );
};
