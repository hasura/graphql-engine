import React from 'react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '@/new-components/Button';
import { FieldArrayWithId } from 'react-hook-form';
import { SortItem, SortRow } from './SortRow';
import { FormValues } from './types';

export type SortRowsProps = {
  columns: SelectItem[];
  fields: FieldArrayWithId<FormValues, 'sort', 'id'>[];
  onAdd: () => void;
  onRemove: (index: number) => void;
  orders: SortItem[];
  showFirstRemove: boolean;
};

export const SortRows = ({
  columns = [],
  fields,
  onAdd,
  onRemove,
  orders,
  showFirstRemove,
}: SortRowsProps) => (
  <div>
    <div className="text-lg font-bold mb-sm">Sort</div>
    <div className="flex flex-col">
      {fields.map((field, index) => (
        <SortRow
          key={field.id}
          rowId={index}
          columns={columns}
          orders={orders}
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
