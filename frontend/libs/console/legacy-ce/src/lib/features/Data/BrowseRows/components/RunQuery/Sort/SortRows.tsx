import { useEffect } from 'react';
import { TableColumn } from '@hasura/metadata/data-source';
import { Flex, Heading } from '@radix-ui/themes';
import { Button, Text } from '@hasura/shared/ui';
import { RiAddBoxLine } from 'react-icons/ri';
import { useFieldArray } from 'react-hook-form';
import { SortRow } from './SortRow';
import { FiltersAndSortFormValues } from '../types';

export type SortRowsProps = {
  columns: TableColumn[];
  name: string;
  initialSorts?: FiltersAndSortFormValues['sorts'];
  onRemove?: () => void;
};

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

export const SortRows = ({
  columns,
  name,
  initialSorts = [],
  onRemove,
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
  }, [initialSorts?.length]);

  const columnOptions = columns
    .sort((a, b) => (a.name > b.name ? 1 : -1))
    .map((column) => {
      return {
        label: column.name,
        value: column.name,
      };
    });

  const removeEntry = (index: number) => {
    remove(index);
    onRemove?.();
  };

  return (
    <div data-testid={`${name}-sort-rows`}>
      <Heading size="3">Sort</Heading>

      {!fields.length && (
        <div className="my-2">
          <Text className="italic">No sort conditions present.</Text>
        </div>
      )}

      <Flex direction="column" gap="2" className="py-2">
        {fields.map((_, index) => (
          <SortRow
            key={index}
            name={`${name}.${index}`}
            columnOptions={columnOptions}
            orderByOptions={orderByOptions}
            onRemove={() => removeEntry(index)}
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
          data-testid="sorts.add"
        >
          Add
        </Button>
      </div>
    </div>
  );
};
