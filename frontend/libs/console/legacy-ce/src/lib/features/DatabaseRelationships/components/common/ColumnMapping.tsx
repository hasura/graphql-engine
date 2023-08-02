import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { FieldError, useFieldArray, useFormContext } from 'react-hook-form';
import { Select } from '../../../../new-components/Form';
import {
  FaArrowAltCircleLeft,
  FaArrowAltCircleRight,
  FaArrowRight,
  FaExclamationCircle,
  FaTimesCircle,
} from 'react-icons/fa';
import { Button } from '../../../../new-components/Button';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { useTableColumns } from '../../hooks/useTableColumns';
import { Schema } from '../ManualLocalRelationship/schema';

const name = 'columnMap';

export const MapColumns = () => {
  const { fields, append } = useFieldArray<Schema>({ name });

  const {
    watch,
    setValue,
    formState: { errors },
  } = useFormContext<Schema>();

  const formErrorMessage = (
    errors?.[name as keyof Schema] as unknown as FieldError
  )?.message;

  const fromSource = watch('fromSource');
  const toSource = watch('toSource');

  const columnMappings = watch(name);
  const {
    data: sourceTableColumns,
    isLoading: areSourceColumnsLoading,
    error: sourceColumnsFetchError,
  } = useTableColumns({
    dataSourceName: fromSource?.value?.dataSourceName,
    table: fromSource?.value?.table,
  });

  const {
    data: targetTableColumns,
    isLoading: areTargetColumnsLoading,
    error: targetColumnsFetchError,
  } = useTableColumns({
    dataSourceName: toSource?.value?.dataSourceName,
    table: toSource?.value?.table,
  });

  if (sourceColumnsFetchError || targetColumnsFetchError)
    return (
      <div className="px-md pb-md mb-md mt-0 h">
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
    <div className="px-md pb-md mb-md mt-0 h">
      <div className="grid grid-cols-12 mb-1 items-center font-semibold text-muted">
        <div className="col-span-6 flex items-center">
          Source Column{' '}
          <FaArrowAltCircleRight className="fill-emerald-700 ml-1.5" />
        </div>
        <div className="col-span-6 flex items-center">
          Reference Column{' '}
          <FaArrowAltCircleLeft className="fill-violet-700 ml-1.5" />
        </div>
      </div>
      {fields.map((field, index) => {
        return (
          <div
            className="grid grid-cols-12 items-center"
            key={`${index}_column_map_row`}
          >
            <div className="col-span-5">
              {areSourceColumnsLoading ? (
                <Skeleton height={35} />
              ) : (
                <Select
                  options={(sourceTableColumns ?? []).map(column => ({
                    label: column.name,
                    value: column.name,
                  }))}
                  name={`${name}.${index}.from`}
                  disabled={!sourceTableColumns?.length}
                  placeholder="Select source column"
                  noErrorPlaceholder
                />
              )}
            </div>

            <div className="flex justify-around">
              <FaArrowRight className="fill-muted" />
            </div>
            <div className="col-span-5">
              {areTargetColumnsLoading ? (
                <Skeleton height={35} />
              ) : (
                <Select
                  options={(targetTableColumns ?? []).map(column => ({
                    label: column.name,
                    value: column.name,
                  }))}
                  name={`${name}.${index}.to`}
                  disabled={!targetTableColumns?.length}
                  placeholder="Select reference column"
                  noErrorPlaceholder
                />
              )}
            </div>
            <div className="flex justify-around">
              <Button
                type="button"
                size="sm"
                className="h-10"
                icon={<FaTimesCircle />}
                onClick={() => {
                  setValue(
                    name,
                    columnMappings.filter((_, i) => index !== i)
                  );
                }}
              />
            </div>
          </div>
        );
      })}
      {formErrorMessage && (
        <div className="text-red-600 mt-1 flex items-center text-sm">
          <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />{' '}
          {formErrorMessage}
        </div>
      )}
      <div className="my-4">
        <Button
          type="button"
          onClick={() => append({})}
          disabled={!targetTableColumns?.length || !sourceTableColumns?.length}
        >
          Add New Mapping
        </Button>
      </div>
    </div>
  );
};
