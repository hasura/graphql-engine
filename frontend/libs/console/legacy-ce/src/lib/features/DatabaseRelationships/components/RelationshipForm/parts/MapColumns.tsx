import React from 'react';
import { FieldError, useFieldArray, useFormContext } from 'react-hook-form';
import { Select } from '../../../../../new-components/Form';
import {
  FaArrowAltCircleLeft,
  FaArrowAltCircleRight,
  FaArrowRight,
  FaExclamationCircle,
  FaTimesCircle,
} from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import { TableColumn } from '../../../../DataSource';
import get from 'lodash/get';

type Schema = Record<
  string,
  {
    from: string;
    to: string;
  }[]
>;

export const MapColumns = ({
  name,
  sourceTableColumns,
  targetTableColumns,
}: {
  name: string;
  sourceTableColumns: TableColumn[];
  targetTableColumns: TableColumn[];
}) => {
  const { fields, append } = useFieldArray<Schema>({ name });

  const {
    watch,
    setValue,
    formState: { errors },
  } = useFormContext<Schema>();

  const maybeError = get(errors, name) as unknown as FieldError;

  const columnMappings: {
    from: string;
    to: string;
  }[] = watch(name);

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
            className="grid grid-cols-12 items-center mb-sm"
            key={`${index}_column_map_row`}
          >
            <div className="col-span-5">
              <Select
                options={sourceTableColumns.map(column => ({
                  label: column.name,
                  value: column.name,
                }))}
                name={`${name}.${index}.from`}
                disabled={!sourceTableColumns?.length}
                placeholder="Select source column"
                noErrorPlaceholder
              />
            </div>

            <div className="flex justify-around">
              <FaArrowRight className="fill-muted" />
            </div>
            <div className="col-span-5">
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
      {maybeError && (
        <div className="text-red-600 mt-1 flex items-center text-sm">
          <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />{' '}
          {maybeError.message}
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
