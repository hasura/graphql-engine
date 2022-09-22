import React from 'react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '@/new-components/Button';
import { DropdownMenu } from '@/new-components/DropdownMenu';
import { FormProvider, useForm } from 'react-hook-form';
import { FaFileExport } from 'react-icons/fa';
import { OperatorItem } from './FilterRow';
import { FilterRows } from './FilterRows';
import { SortItem } from './SortRow';
import { SortRows } from './SortRows';
import {
  defaultColumn,
  defaultOperator,
  defaultOrder,
  FiltersAndSortFormValues,
} from './types';
import { useFilterRows } from './hooks/useFilterRows';
import { useSortRows } from './hooks/useSortRows';

export const sortPlaceholder = '--';

export const sortOptions: SortItem[] = [
  {
    label: sortPlaceholder,
    value: sortPlaceholder,
    disabled: true,
  },
  {
    label: 'Asc',
    value: 'asc',
  },
  {
    label: 'Desc',
    value: 'desc',
  },
];

type FiltersSectionProps = {
  columns: SelectItem[];
  operators: OperatorItem[];
  orders: SortItem[];
  onExport: (
    type: 'CSV' | 'JSON',
    formValues: FiltersAndSortFormValues
  ) => void;
  onSubmit: (values: FiltersAndSortFormValues) => void;
};

export const FiltersSection = ({
  columns,
  operators,
  orders,
  onExport,
  onSubmit,
}: FiltersSectionProps) => {
  const methods = useForm<FiltersAndSortFormValues>({
    defaultValues: {
      filter: [{ column: defaultColumn, operator: defaultOperator, value: '' }],
      sort: [{ column: defaultColumn, order: defaultOrder }],
    },
  });

  const { handleSubmit } = methods;

  const {
    onRemoveFilterRow,
    onAddFilterRow,
    filterFields,
    showFirstRemoveOnFilter,
  } = useFilterRows({ methods, operators });

  const { onRemoveSortRow, onAddSortRow, sortFields, showFirstRemoveOnSort } =
    useSortRows({ methods });

  const exportItems = [
    [<div onClick={handleSubmit(arg => onExport('CSV', arg))}>CSV</div>],
    [<div onClick={handleSubmit(arg => onExport('JSON', arg))}>JSON</div>],
  ];

  return (
    <FormProvider {...methods}>
      <form onSubmit={handleSubmit(onSubmit)}>
        <div className="flex flex-col w-full">
          <div className="flex flex-row">
            <div className="flex-grow w-1/2 pr-4">
              <FilterRows
                columns={columns}
                fields={filterFields}
                onAdd={onAddFilterRow}
                onRemove={onRemoveFilterRow}
                operators={operators}
                showFirstRemove={showFirstRemoveOnFilter}
              />
            </div>
            <div className="flex-grow w-1/2 pr-4">
              <SortRows
                columns={columns}
                fields={sortFields}
                onAdd={() => onAddSortRow()}
                onRemove={index => onRemoveSortRow(index)}
                orders={orders}
                showFirstRemove={showFirstRemoveOnSort}
              />
            </div>
          </div>
          <div className="flex mt-4">
            <Button mode="primary" className="mr-4" type="submit">
              Run query
            </Button>
            <div>
              <DropdownMenu items={exportItems}>
                <Button icon={<FaFileExport />} iconPosition="start">
                  Export data
                </Button>
              </DropdownMenu>
            </div>
          </div>
        </div>
      </form>
    </FormProvider>
  );
};
