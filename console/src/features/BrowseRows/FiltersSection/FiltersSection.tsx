import React from 'react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import ExportData, {
  ExportDataProps,
} from '@/components/Services/Data/TableBrowseRows/ExportData';
import { Button } from '@/new-components/Button';
import { FormProvider, useForm } from 'react-hook-form';
import { OperatorItem } from './FilterRow';
import { FilterRows } from './FilterRows';
import { SortItem } from './SortRow';
import { SortRows } from './SortRows';
import {
  defaultColumn,
  defaultOperator,
  defaultOrder,
  FormValues,
} from './types';
import { useFilterRows } from './hooks/useFilterRows';
import { useSortRows } from './hooks/useSortRows';

/* 
  NOTE:
  Component created out of from FilterQuery.
  We'll need to delete FilterQuery in the future, when the integration of Redux is complete
  and we'll integrate FiltersSection in the Browse Rows tab.
*/

type FiltersSectionProps = {
  columns: SelectItem[];
  operators: OperatorItem[];
  orders: SortItem[];
  onExport: ExportDataProps['onExport'];
  onSubmit: (values: FormValues) => void;
};

export const FiltersSection = ({
  columns,
  operators,
  orders,
  onExport,
  onSubmit,
}: FiltersSectionProps) => {
  const methods = useForm<FormValues>({
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
              {/* TODO: fix the dropdown */}
              <ExportData onExport={onExport} />
            </div>
          </div>
        </div>
      </form>
    </FormProvider>
  );
};
