import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '@/new-components/Button';
import { Select } from '@/new-components/Form';
import React from 'react';
import { FaTimes } from 'react-icons/fa';

export type SortItem = SelectItem & { defaultValue?: string };

type FilterRowProps = {
  rowId: number;
  columns: SelectItem[];
  orders: SortItem[];
  onRemove: () => void;
  showRemove: boolean;
};

export const SortRow = ({
  rowId,
  columns,
  orders,
  onRemove,
  showRemove,
}: FilterRowProps) => (
  <div className="flex -mb-3">
    <div className="w-full">
      <Select name={`sort.${rowId}.column`} options={columns} />
    </div>
    <div className="w-full ml-xs">
      <Select name={`sort.${rowId}.order`} options={orders} />
    </div>
    <div className="w-1/10 justify-center flex items-center mb-6 ml-1">
      <Button
        size="sm"
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
