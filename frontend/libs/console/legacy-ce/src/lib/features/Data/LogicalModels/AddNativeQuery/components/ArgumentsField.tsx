import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import clsx from 'clsx';
import React, { useRef } from 'react';
import { Controller, useFieldArray, useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import {
  FieldWrapper,
  GraphQLSanitizedInputField,
  InputField,
  Select,
  fieldLabelStyles,
} from '../../../../../new-components/Form';
import { Switch } from '../../../../../new-components/Switch';
import { useCardedTableFromReactTableWithRef } from '../../components/CardedTableFromReactTable';
import { NativeQueryArgumentNormalized, NativeQueryForm } from '../types';

const columnHelper = createColumnHelper<NativeQueryArgumentNormalized>();

export const ArgumentsField = () => {
  const { control } = useFormContext<NativeQueryForm>();

  const { append, remove, fields } = useFieldArray({
    control,
    name: 'arguments',
  });

  const tableRef = useRef<HTMLDivElement>(null);

  const argumentColumns = React.useMemo(
    () => [
      columnHelper.accessor('name', {
        id: 'name',
        cell: ({ row }) => (
          <GraphQLSanitizedInputField
            noErrorPlaceholder
            hideTips
            placeholder="Parameter Name"
            name={`arguments.${row.index}.name`}
          />
        ),
        header: 'Name',
      }),
      columnHelper.accessor('type', {
        id: 'type',
        cell: ({ row }) => (
          <Select
            noErrorPlaceholder
            // saving prop for future upgrade
            //menuPortalTarget={tableRef.current}
            name={`arguments.${row.index}.type`}
            options={[
              { value: 'string', label: 'string' },
              { value: 'int', label: 'int' },
            ]}
          />
        ),
        header: 'Type',
      }),
      columnHelper.accessor('default_value', {
        id: 'default_value',
        cell: ({ row }) => (
          <InputField
            noErrorPlaceholder
            placeholder="Default Value"
            name={`arguments.${row.index}.default_value`}
          />
        ),
        header: 'Default Value',
      }),
      columnHelper.accessor('required', {
        id: 'required',
        cell: ({ row }) => (
          <Controller
            name={`arguments.${row.index}.required`}
            render={({ field: { value, onChange }, fieldState }) => (
              <FieldWrapper
                id={`arguments.${row.index}.required`}
                noErrorPlaceholder
                error={fieldState.error}
              >
                <Switch
                  data-testid="required-switch"
                  checked={value}
                  onCheckedChange={onChange}
                />
              </FieldWrapper>
            )}
          />
        ),
        header: 'Required',
      }),
      columnHelper.display({
        id: 'action',
        header: 'Actions',
        cell: ({ row }) => (
          <div className="flex flex-row gap-2">
            <Button mode="destructive" onClick={() => remove(row.index)}>
              Remove
            </Button>
          </div>
        ),
      }),
    ],
    [control, remove]
  );

  const argumentsTable = useReactTable({
    data: fields,
    columns: argumentColumns,
    getCoreRowModel: getCoreRowModel(),
  });

  const ArgumentsTableElement =
    useCardedTableFromReactTableWithRef<NativeQueryArgumentNormalized>();

  return (
    <div>
      <div className="flex flex-col gap-2 ">
        <div className="flex justify-between items-center">
          <div className={clsx(fieldLabelStyles, 'mb-0')}>Query Parameters</div>
          <Button
            icon={<FaPlusCircle />}
            onClick={() => {
              append({
                name: '',
                type: 'string',
              });
            }}
          >
            Add Parameter
          </Button>
        </div>
        <ArgumentsTableElement
          table={argumentsTable}
          ref={tableRef}
          noRowsMessage={'No query parameters added.'}
        />
      </div>
    </div>
  );
};
