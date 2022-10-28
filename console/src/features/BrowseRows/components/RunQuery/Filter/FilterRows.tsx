import React from 'react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '@/new-components/Button';
import { Operator, TableColumn, WhereClause } from '@/features/DataSource';
import { RiAddBoxLine } from 'react-icons/ri';
import { useFieldArray } from 'react-hook-form';
import { FilterRow } from './FilterRow';

export type FilterRowsProps = {
  columns: TableColumn[];
  operators: Operator[];
  name: string;
};

export const FilterRows = ({ name, columns, operators }: FilterRowsProps) => {
  const { fields, append, remove } = useFieldArray<
    Record<string, WhereClause[]>
  >({
    name,
  });

  const removeEntry = (index: number) => {
    remove(index);
  };

  const columnOptions: SelectItem[] = columns.map(column => {
    const value = column.graphQLProperties?.name ?? column.name;
    return {
      label: column.name,
      value,
    };
  });

  const operatorOptions: SelectItem[] = operators.map(operator => ({
    label: `[${operator.value}] ${operator.name}`,
    value: operator.value,
  }));

  const defaultValues = operators
    .filter(operator => !!operator.defaultValue)
    .reduce((acc, operator) => {
      return {
        ...acc,
        [operator.value]: operator.defaultValue,
      };
    }, {});

  return (
    <div data-testid={`${name}-filter-rows`}>
      <div className="text-lg font-semibold mb-sm">Filters</div>

      {!fields.length && <div className="mb-sm italic">No Filters Present</div>}

      <div className="flex flex-col">
        {fields.map((_, index) => (
          <FilterRow
            key={index}
            columnOptions={columnOptions}
            operatorOptions={operatorOptions}
            onRemove={() => {
              removeEntry(index);
            }}
            name={`${name}.${index}`}
            defaultValues={defaultValues}
          />
        ))}
      </div>
      <div>
        <Button
          type="button"
          size="sm"
          onClick={() => append({})}
          icon={<RiAddBoxLine />}
          data-testid={`${name}.add`}
        >
          Add
        </Button>
      </div>
    </div>
  );
};
