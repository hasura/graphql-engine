import React, { useEffect } from 'react';
import { Button } from '@/new-components/Button';
import { DropdownMenu } from '@/new-components/DropdownMenu';
import { Operator, TableColumn } from '@/features/DataSource';
import { FormProvider, useForm } from 'react-hook-form';
import { FaFileExport } from 'react-icons/fa';
import { FilterRows } from '../Filter';
import { SortRows } from '../Sort';
import { FiltersAndSortFormValues } from '../types';
import { getUrlQueryParams } from './LegacyRunQueryContainer';

type LegacyRunQueryProps = {
  columns: TableColumn[];
  operators: Operator[];
  onExport: (
    type: 'CSV' | 'JSON',
    formValues: FiltersAndSortFormValues
  ) => void;
  onSubmit: (values: FiltersAndSortFormValues) => void;
  initialUserQuery?: FiltersAndSortFormValues;
  uniqueTableName?: string;
};
const defaultQueryValues: FiltersAndSortFormValues = {
  filter: [],
  sort: [],
};
export const LegacyRunQuery = ({
  onExport,
  onSubmit,
  operators,
  columns,
  initialUserQuery = defaultQueryValues,
  uniqueTableName = '',
}: LegacyRunQueryProps) => {
  const methods = useForm<FiltersAndSortFormValues>({
    defaultValues: initialUserQuery,
  });

  const { handleSubmit, reset } = methods;

  useEffect(() => {
    const queryParams = getUrlQueryParams();
    if (queryParams.filter.length > 0 || queryParams.sort.length > 0) {
      return;
    }

    reset(defaultQueryValues);
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
                name="filter"
                initialFilters={initialUserQuery.filter}
              />
            </div>
            <div className="flex-grow w-1/2 pr-4">
              <SortRows
                columns={columns}
                name="sort"
                initialSorts={initialUserQuery.sort}
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
