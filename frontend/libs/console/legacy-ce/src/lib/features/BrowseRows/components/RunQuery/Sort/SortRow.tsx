import { SelectItem } from '../../../../../components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '../../../../../new-components/Button';
import { Select } from '../../../../../new-components/Form';
import React from 'react';
import { FaTimes } from 'react-icons/fa';

export type SortItem = SelectItem & { defaultValue?: string };

type SortRowProps = {
  name: string;
  columnOptions: SelectItem[];
  orderByOptions: SelectItem[];
  onRemove: () => void;
};

export const SortRow = ({
  name,
  columnOptions,
  orderByOptions,
  onRemove,
}: SortRowProps) => (
  <div className="flex space-x-4">
    <Select
      name={`${name}.column`}
      options={columnOptions}
      placeholder="Select a column"
      data-testid={`${name}.column`}
    />
    <Select
      name={`${name}.type`}
      options={orderByOptions}
      placeholder="Order by"
      data-testid={`${name}.type`}
    />

    <Button
      icon={<FaTimes />}
      className="mr-1"
      disabled={false}
      onClick={onRemove}
      data-testid={`${name}.remove`}
    />
  </div>
);
