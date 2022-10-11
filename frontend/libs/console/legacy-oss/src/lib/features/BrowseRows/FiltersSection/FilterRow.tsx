import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '@/new-components/Button';
import { InputField, Select } from '@/new-components/Form';
import React from 'react';
import { FaTimes } from 'react-icons/fa';

export type OperatorItem = SelectItem & { defaultValue?: string };

type FilterRowProps = {
  columns: SelectItem[];
  onRemove: () => void;
  operators: OperatorItem[];
  rowId: number;
  showRemove: boolean;
};

export const FilterRow = ({
  columns,
  onRemove,
  operators,
  rowId,
  showRemove,
}: FilterRowProps) => (
  <div className="flex -mb-3">
    <div className="w-full">
      <Select name={`filter.${rowId}.column`} options={columns} />
    </div>
    <div className="w-8/12 ml-xs">
      <Select name={`filter.${rowId}.operator`} options={operators} />
    </div>
    <div className="w-8/12 ml-xs">
      <InputField name={`filter.${rowId}.value`} placeholder="-- value --" />
    </div>
    <div className="w-1/12 justify-center flex items-center mb-6 ml-1">
      <Button
        size="sm"
        mode="default"
        icon={<FaTimes />}
        className="mr-1"
        disabled={false}
        onClick={onRemove}
        data-test={`row-button-${rowId}`}
        style={{
          visibility: showRemove ? 'visible' : 'hidden',
        }}
      />
    </div>
  </div>
);
