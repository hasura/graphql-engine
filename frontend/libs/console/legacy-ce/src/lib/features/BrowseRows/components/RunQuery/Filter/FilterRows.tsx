import React, { useEffect } from 'react';
import { SelectItem } from '../../../../../components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '../../../../../new-components/Button';
import { Operator, TableColumn, WhereClause } from '../../../../DataSource';
import { RiAddBoxLine } from 'react-icons/ri';
import { useFieldArray } from 'react-hook-form';
import { FilterRow } from './FilterRow';
import { FiltersAndSortFormValues } from '../types';

export type FilterRowsProps = {
  columns: TableColumn[];
  operators: Operator[];
  name: string;
  initialFilters?: FiltersAndSortFormValues['filters'];
  onRemove?: () => void;
};

export const FilterRows = ({
  name,
  columns,
  operators,
  initialFilters = [],
  onRemove,
}: FilterRowsProps) => {
  const { fields, append, remove, update } = useFieldArray<
    Record<string, WhereClause[]>
  >({
    name,
  });

  useEffect(() => {
    if (initialFilters.length > 0) {
      initialFilters.forEach((filter, index) => {
        // TODO-NEXT: find a way to fix the types below
        update(index, {
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          column: filter.column,
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          operator: filter.operator,
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          value: filter.value,
        });
      });
    }
  }, [initialFilters?.length]);

  const removeEntry = (index: number) => {
    remove(index);
    onRemove?.();
  };

  const columnOptions: SelectItem[] = columns.map(column => {
    return {
      label: column.name,
      value: column.name,
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

      <div className="flex flex-col gap-2 pb-2">
        {fields.map((_, index) => (
          <FilterRow
            key={index}
            columnOptions={columnOptions}
            operatorOptions={operatorOptions}
            onRemove={() => removeEntry(index)}
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
