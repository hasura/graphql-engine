import React, { useEffect } from 'react';
import { Button } from '../../../../../new-components/Button';
import { DropdownMenu } from '../../../../../new-components/DropdownMenu';
import { Operator, TableColumn } from '../../../../DataSource';
import { FormProvider, useForm } from 'react-hook-form';
import { FaFileExport } from 'react-icons/fa';
import { FilterRows } from '../Filter';
import { SortRows } from '../Sort';
import { FiltersAndSortFormValues } from '../types';
import { getFiltersAndSortFromUrlQueryParams } from './LegacyRunQueryContainer';

type LegacyRunQueryProps = {
  columns: TableColumn[];
  operators: Operator[];
  onExport: (
    type: 'CSV' | 'JSON',
    formValues: FiltersAndSortFormValues
  ) => void;
  onSubmit: (values: FiltersAndSortFormValues) => void;
  initialFiltersAndSort?: FiltersAndSortFormValues;
  uniqueTableName?: string;
};

export const defaultFiltersAndSortFormValues: FiltersAndSortFormValues = {
  filters: [],
  sorts: [],
};

export const LegacyRunQuery = ({
  onExport,
  onSubmit,
  operators,
  columns,
  initialFiltersAndSort = defaultFiltersAndSortFormValues,
  uniqueTableName = '',
}: LegacyRunQueryProps) => {
  const methods = useForm<FiltersAndSortFormValues>({
    defaultValues: initialFiltersAndSort,
  });

  const { handleSubmit, reset } = methods;

  useEffect(() => {
    const filtersAndSortFromUrl = getFiltersAndSortFromUrlQueryParams();
    if (
      filtersAndSortFromUrl.filters.length > 0 ||
      filtersAndSortFromUrl.sorts.length > 0
    ) {
      return;
    }

    reset(initialFiltersAndSort);
  }, [uniqueTableName]);

  const exportItems = [
    [<div onClick={handleSubmit(arg => onExport('CSV', arg))}>CSV</div>],
    [<div onClick={handleSubmit(arg => onExport('JSON', arg))}>JSON</div>],
  ];

  return (
    <FormProvider {...methods}>
      <form onSubmit={handleSubmit(onSubmit)}>
        <div className="flex flex-col w-full">
          <div className="flex">
            <div className="flex-grow w-1/2 pr-4">
              <FilterRows
                columns={columns}
                operators={operators}
                name="filters"
                initialFilters={initialFiltersAndSort.filters}
                onRemove={() => handleSubmit(onSubmit)()}
              />
            </div>
            <div className="flex-grow w-1/2 pr-4">
              <SortRows
                columns={columns}
                name="sorts"
                initialSorts={initialFiltersAndSort.sorts}
                onRemove={() => handleSubmit(onSubmit)()}
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
