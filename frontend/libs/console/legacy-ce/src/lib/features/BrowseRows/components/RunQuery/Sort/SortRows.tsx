import React, { useEffect } from 'react';
import { TableColumn } from '@/features/DataSource';
import { Button } from '@/new-components/Button';
import { RiAddBoxLine } from 'react-icons/ri';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { useFieldArray } from 'react-hook-form';
import { SortRow } from './SortRow';
import { FiltersAndSortFormValues } from '../types';

export type SortRowsProps = {
  columns: TableColumn[];
  name: string;
  initialSorts?: FiltersAndSortFormValues['sort'];
};

export const SortRows = ({
  columns,
  name,
  initialSorts = [],
}: SortRowsProps) => {
  const { fields, append, remove, update } = useFieldArray({
    name,
  });

  useEffect(() => {
    if (initialSorts.length > 0) {
      initialSorts.forEach((sort, index) => {
        update(index, { column: sort.column, type: sort.type });
      });
    }
  }, [initialSorts]);

  const columnOptions: SelectItem[] = columns.map(column => {
    const value = column.graphQLProperties?.name ?? column.name;
    return {
      label: column.name,
      value,
    };
  });

  const orderByOptions = [
    {
      label: 'Asc',
      value: 'asc',
    },
    {
      label: 'Desc',
      value: 'desc',
    },
  ];

  const removeEntry = (index: number) => {
    remove(index);
  };

  return (
    <div data-testid={`${name}-sort-rows`}>
      <div className="text-lg font-bold mb-sm">Sort</div>

      {!fields.length && (
        <div className="mb-sm italic">No sort conditions present.</div>
      )}

      <div className="flex flex-col">
        {fields.map((_, index) => (
          <SortRow
            key={index}
            name={`${name}.${index}`}
            columnOptions={columnOptions}
            orderByOptions={orderByOptions}
            onRemove={() => removeEntry(index)}
          />
        ))}
      </div>
      <div>
        <Button
          type="button"
          size="sm"
          onClick={() => append({})}
          icon={<RiAddBoxLine />}
          data-testid="sorts.add"
        >
          Add
        </Button>
      </div>
    </div>
  );
};
