import React, { useRef } from 'react';
import Truncate from 'react-truncate';

import { TypedInput } from './TypedInput';
import { TableColumn } from '../../../../../dataSources/types';
import { focusYellowRing } from '../../constants';
import { isColumnAutoIncrement } from './utils';
import { IconTooltip } from '../../../../../new-components/Tooltip/IconTooltip';
import { FaEye } from 'react-icons/fa';

const getColumnInfo = (
  col: TableColumn,
  prevValue?: unknown,
  clone?: Record<string, unknown>
) => {
  const isEditing = prevValue !== undefined;

  const hasDefault = col.column_default
    ? col.column_default.trim() !== ''
    : false;

  const isAutoIncrement = isColumnAutoIncrement(col);
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
  enumOptions: string[];
  index: string;
  onChange?: (e: React.ChangeEvent<HTMLInputElement>) => void;
  onFocus?: (e: React.FocusEvent<HTMLInputElement>) => void;
  prevValue?: unknown;
  values: Record<string, unknown>;
  setNullCheckedValues: (colName: string, isNullChecked: boolean) => void;
  setDefaultValueColumns: (colName: string, isNullChecked: boolean) => void;
}

export const TableRow: React.FC<TableRowProps> = ({
  column,
  onChange,
  enumOptions,
  index,
  prevValue,
  values,
  setNullCheckedValues,
  setDefaultValueColumns,
}) => {
  const { colName, isDisabled, isNullable, hasDefault, columnValueType } =
    getColumnInfo(column, prevValue, values);

  const valueRadioRef = useRef<HTMLInputElement>(null);
  const nullRadioRef = useRef<HTMLInputElement>(null);
  const defaultRadioRef = useRef<HTMLInputElement>(null);
  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (isDisabled) return;
    if (valueRadioRef.current) {
      valueRadioRef.current.checked = true;
    }
    if (nullRadioRef.current) {
      nullRadioRef.current.checked = false;
    }
    if (defaultRadioRef.current) {
      defaultRadioRef.current.checked = false;
    }
    if (onChange) {
      onChange(e);
    }
  };

  return (
    <div className="items-center flex pb-xs">
      <div className="w-2/12 relative mr-1">
        <Truncate
          lines={1}
          ellipsis={
            <span>
              ...
              <div className="absolute top-[4px] -left-8 opacity-60 hover:opacity-100">
                <IconTooltip icon={<FaEye />} message={colName} />
              </div>
            </span>
          }
        >
          {colName}
        </Truncate>
      </div>
      <div>
        <input
          type="radio"
          ref={valueRadioRef}
          className={`${focusYellowRing} !m-0 !mr-sm cursor-pointer`}
          onChange={() => {
            setNullCheckedValues(colName, false);
            setDefaultValueColumns(colName, false);
          }}
          name={`${colName}-value`}
          defaultChecked={columnValueType === 'value'}
          disabled={isDisabled}
        />
      </div>
      <div className="flex items-center mr-sm relative">
        <TypedInput
          values={values}
          prevValue={prevValue as null}
          enumOptions={enumOptions}
          col={column}
          onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
            handleChange(e);
            setNullCheckedValues(colName, false);
            setDefaultValueColumns(colName, false);
          }}
          disabled={isDisabled}
          index={index}
        />
      </div>
      <div className="flex items-center mr-sm relative">
        <input
          type="radio"
          className={`${focusYellowRing} !m-0 !mr-sm cursor-pointer`}
          ref={nullRadioRef}
          disabled={!isNullable}
          defaultChecked={columnValueType === 'null'}
          onChange={() => {
            setNullCheckedValues(colName, true);
            setDefaultValueColumns(colName, false);
          }}
          name={`${colName}-value`}
          data-test={`null-value-radio-${index}`}
          data-testid={`null-value-radio-${index}`}
        />
        <div>NULL</div>
      </div>
      <div className="flex items-center mr-sm relative">
        <input
          type="radio"
          ref={defaultRadioRef}
          className={`${focusYellowRing} !m-0 !mr-sm cursor-pointer`}
          onChange={() => {
            setNullCheckedValues(colName, false);
            setDefaultValueColumns(colName, true);
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
