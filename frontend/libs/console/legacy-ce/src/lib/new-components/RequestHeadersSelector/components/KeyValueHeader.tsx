import React from 'react';
import { RiCloseCircleFill } from 'react-icons/ri';
import { useFormContext } from 'react-hook-form';

interface Props {
  fieldId: string;
  fieldName: string;
  rowIndex: number;
  typeSelect: boolean;
  removeRow: (index?: number | number[]) => void;
}

const borderStyle =
  'border-gray-300 hover:border-gray-400 focus:border-yellow-400';
const ringStyle = 'focus:ring-2 focus:ring-yellow-200';

export const KeyValueHeader = (props: Props) => {
  const { fieldId, fieldName, rowIndex, typeSelect, removeRow } = props;
  const keyLabel = `${fieldName}[${rowIndex}].name`;
  const typeLabel = `${fieldName}[${rowIndex}].type`;
  const valueLabel = `${fieldName}[${rowIndex}].value`;
  const { register } = useFormContext();

  return (
    <div className="grid gap-4 grid-cols-2 mb-sm" key={fieldId}>
      <input
        type="text"
        className={`w-full block h-10 shadow-sm rounded ${borderStyle} ${ringStyle}`}
        placeholder="Key..."
        {...register(keyLabel)}
        aria-label={keyLabel}
        data-test={`header-test${rowIndex}-key`}
      />
      <div className="flex rounded">
        {typeSelect ? (
          <select
            {...register(typeLabel)}
            aria-label={typeLabel}
            className={`inline-flex h-10 shadow-sm rounded-l border border-r-0 bg-white ${borderStyle} ${ringStyle}`}
          >
            <option value="from_value">Value</option>
            <option value="from_env">Env Var</option>
          </select>
        ) : null}
        <input
          {...register(valueLabel)}
          aria-label={valueLabel}
          data-test={`header-test${rowIndex}-value`}
          type="text"
          className={`flex-1 min-w-0 h-10 shadow-sm block w-full px-3 py-2 rounded-r ${borderStyle} ${ringStyle}`}
          placeholder="Value..."
        />
        <div className="col-span-1 flex items-center justify-center pl-1.5">
          <RiCloseCircleFill
            onClick={() => {
              removeRow(rowIndex);
            }}
            className="cursor-pointer"
            data-test={`delete-header-${rowIndex}`}
          />
        </div>
      </div>
    </div>
  );
};
