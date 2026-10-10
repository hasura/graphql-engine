import { useEffect } from 'react';
import { Flex, Heading } from '@radix-ui/themes';
import { Button, ReactSelectOptionType, Text } from '@hasura/shared/ui';
import { Operator, TableColumn } from '@hasura/metadata/data-source';
import { RiAddBoxLine } from 'react-icons/ri';
import { useFieldArray } from 'react-hook-form';
import { FilterRow } from './FilterRow';
import { FiltersAndSortFormValues } from '../types';
import { WhereClause } from '@hasura/shared/types';

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

  const columnOptions: ReactSelectOptionType[] = columns
    .sort((a, b) => (a.name > b.name ? 1 : -1))
    .map((column) => {
      return {
        label: column.name,
        value: column.name,
      };
    });

  const operatorOptions: ReactSelectOptionType[] = operators.map(
    (operator) => ({
      label: `[${operator.value}] ${operator.name}`,
      value: operator.value,
    }),
  );

  const defaultValues = operators
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
            operatorOptions={operatorOptions}
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
          onClick={() => append({})}
          leftIcon={RiAddBoxLine}
          data-testid={`${name}.add`}
        >
          Add
        </Button>
      </div>
    </div>
  );
};
