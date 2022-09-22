import React from 'react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '@/new-components/Button';
import { FieldArrayWithId } from 'react-hook-form';
import { FilterRow, OperatorItem } from './FilterRow';
import { FiltersAndSortFormValues } from './types';

export type FilterRowsProps = {
  columns: SelectItem[];
  fields: FieldArrayWithId<FiltersAndSortFormValues, 'filter', 'id'>[];
  onAdd: () => void;
  onRemove: (index: number) => void;
  operators: OperatorItem[];
  showFirstRemove: boolean;
};

export const FilterRows = ({
  columns = [],
  fields,
  onAdd,
  onRemove,
  operators,
  showFirstRemove,
}: FilterRowsProps) => {
  return (
    <div>
      <div className="text-lg font-bold mb-sm">Filter</div>
      <div className="flex flex-col">
        {fields.map((field, index) => (
          <FilterRow
            key={field.id}
            rowId={index}
            columns={columns}
            operators={operators}
            onRemove={() => onRemove(index)}
            showRemove={index > 0 || showFirstRemove}
          />
        ))}
      </div>
      <div>
        <Button type="button" size="sm" onClick={() => onAdd()}>
          Add
        </Button>
      </div>
    </div>
  );
};
