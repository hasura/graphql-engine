import { useEffect } from 'react';
import { Flex, Heading } from '@radix-ui/themes';
import { Button, ReactSelectOptionType, Text } from '@hasura/shared/ui';
import { Operator, TableColumn } from '@hasura/metadata/data-source';
import { RiAddBoxLine } from 'react-icons/ri';
import { useFieldArray } from 'react-hook-form';
import { FilterRow } from './FilterRow';
import { FilterFormValues, FiltersAndSortFormValues } from '../types';

export type FilterRowsProps = {
  columns: TableColumn[];
  /** operators of a column whose operators aren't known */
  operators: Operator[];
  /** the operators each column supports, by column name */
  columnOperators?: Record<string, Operator[]>;
  /** the allowed values of the columns that reference an enum table */
  enumColumnValues?: Record<string, string[]>;
  name: string;
  initialFilters?: FiltersAndSortFormValues['filters'];
  onRemove?: () => void;
};

export const FilterRows = ({
  name,
  columns,
  operators,
  columnOperators,
  enumColumnValues,
  initialFilters = [],
  onRemove,
}: FilterRowsProps) => {
  const { fields, append, remove, update } = useFieldArray<
    Record<string, FilterFormValues[]>
  >({
    name,
  });

  useEffect(() => {
    initialFilters.forEach(({ column, operator, value }, index) => {
      update(index, { column, operator, value });
    });
  }, [initialFilters, update]);

  const removeEntry = (index: number) => {
    remove(index);
    onRemove?.();
  };

  // Copy before sorting: `columns` is the array held in the introspection
  // query cache, and `Array.prototype.sort` sorts in place. Sorting it here
  // used to leave the cached columns alphabetically ordered, so the next
  // browse query selected (and therefore displayed) them alphabetically
  // instead of in table order.
  const columnOptions: ReactSelectOptionType[] = [...columns]
    .sort((a, b) => (a.name > b.name ? 1 : -1))
    .map((column) => {
      return {
        label: column.name,
        value: column.name,
      };
    });

  const defaultValues = [
    ...operators,
    ...Object.values(columnOperators ?? {}).flat(),
  ]
    .filter((operator) => !!operator.defaultValue)
    .reduce((acc, operator) => {
      return {
        ...acc,
        [operator.value]: operator.defaultValue,
      };
    }, {});

  return (
    <div data-testid={`${name}-filter-rows`}>
      <Heading size="3">Filters</Heading>

      {!fields.length && (
        <div className="my-2">
          <Text className="italic">No Filters Present</Text>
        </div>
      )}

      <Flex direction="column" gap="2" className="my-2">
        {fields.map((_, index) => (
          <FilterRow
            key={index}
            columnOptions={columnOptions}
            columns={columns}
            operators={operators}
            columnOperators={columnOperators}
            enumColumnValues={enumColumnValues}
            onRemove={() => removeEntry(index)}
            name={`${name}.${index}`}
            defaultValues={defaultValues}
          />
        ))}
      </Flex>
      <div>
        <Button
          type="button"
          size="sm"
          mode="default"
          onClick={() => append({ column: '', operator: '', value: '' })}
          leftIcon={RiAddBoxLine}
          data-testid={`${name}.add`}
        >
          Add
        </Button>
      </div>
    </div>
  );
};
