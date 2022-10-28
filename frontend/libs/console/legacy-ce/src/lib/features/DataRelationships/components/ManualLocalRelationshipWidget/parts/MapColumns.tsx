import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { FieldError, useFieldArray, useFormContext } from 'react-hook-form';
import { Select } from '@/new-components/Form';
import { FaArrowRight, FaCircle, FaTrashAlt } from 'react-icons/fa';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Schema } from '../schema';
import { useTableColumns } from '../hooks/useTableColumns';

export const MapColumns = () => {
  const { fields, append } = useFieldArray<Schema>({ name: 'column_mapping' });

  const {
    watch,
    setValue,
    formState: { errors },
  } = useFormContext<Schema>();
  const formErrorMessage = (errors?.column_mapping as unknown as FieldError)
    ?.message;
  const sourceTable = watch('source_table');
  const sourceDataSourceName = watch('source_name');
  const targetTable = watch('target_table');
  const targetDataSourceName = watch('target_name');

  const columnMappings = watch('column_mapping');
  const {
    data: sourceTableColumns,
    isLoading: areSourceColumnsLoading,
    error: sourceColumnsFetchError,
  } = useTableColumns({
    dataSourceName: sourceDataSourceName,
    table: sourceTable,
  });

  const {
    data: targetTableColumns,
    isLoading: areTargetColumnsLoading,
    error: targetColumnsFetchError,
  } = useTableColumns({
    dataSourceName: targetDataSourceName,
    // This condition is wait for targetTable to get set/unset, until then `undefined` is passed to react-query, which will stop the
    //  unnessacary call from being made
    table: targetTable ? JSON.parse(targetTable) : undefined,
  });

  if (sourceColumnsFetchError || targetColumnsFetchError)
    return (
      <div className="rounded bg-gray-50 border border-gray-300 p-md mb-md mt-0 h">
        <div className="items-center mb-sm font-semibold text-gray-600">
          <IndicatorCard
            status="negative"
            headline="Errors while fetching columns"
            showIcon
          >
            <ul>
              {!!sourceColumnsFetchError && (
                <li>
                  source table error:{' '}
                  {JSON.stringify(sourceColumnsFetchError.response?.data)}
                </li>
              )}
              {!!targetColumnsFetchError && (
                <li>
                  target table error:
                  {JSON.stringify(targetColumnsFetchError.response?.data)}
                </li>
              )}
            </ul>
          </IndicatorCard>
        </div>
      </div>
    );

  return (
    <div className="rounded bg-gray-50 border border-gray-300 p-md mb-md mt-0 h">
      <div className="grid grid-cols-12 items-center mb-sm font-semibold text-gray-600">
        <div className="col-span-6">
          <FaCircle className="text-green-600" /> Source Column
        </div>
        <div className="col-span-6">
          <FaCircle className="text-indigo-600" /> Reference Column
        </div>
      </div>
      <div className="text-red-500">{formErrorMessage}</div>
      {fields.map((field, index) => {
        return (
          <div
            className="grid grid-cols-12 items-center mb-sm"
            key={`${index}_column_map_row`}
          >
            <div className="col-span-5">
              {areSourceColumnsLoading ? (
                <Skeleton height={35} />
              ) : (
                <Select
                  options={(sourceTableColumns ?? []).map((column) => ({
                    label: column.name,
                    value: column.name,
                  }))}
                  name={`column_mapping.${index}.from`}
                  placeholder="Select source column"
                  noErrorPlaceholder={false}
                />
              )}
            </div>

            <div className="flex justify-around">
              <FaArrowRight />
            </div>
            <div className="col-span-5">
              {areTargetColumnsLoading ? (
                <Skeleton height={35} />
              ) : (
                <Select
                  options={(targetTableColumns ?? []).map((column) => ({
                    label: column.name,
                    value: column.name,
                  }))}
                  name={`column_mapping.${index}.to`}
                  placeholder="Select reference column"
                  noErrorPlaceholder={false}
                />
              )}
            </div>
            <div className="flex justify-around">
              <Button
                type="button"
                icon={<FaTrashAlt />}
                onClick={() => {
                  setValue(
                    'column_mapping',
                    columnMappings.filter((_, i) => index !== i)
                  );
                }}
              />
            </div>
          </div>
        );
      })}
      <div className="my-4">
        <Button type="button" onClick={() => append({})}>
          Add New Mapping
        </Button>
      </div>
    </div>
  );
};
