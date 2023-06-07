import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import clsx from 'clsx';
import React, { useRef } from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import {
  GraphQLSanitizedInputField,
  InputField,
  Select,
  fieldLabelStyles,
} from '../../../../../new-components/Form';
import { BooleanInput } from '../../components/BooleanInput';
import { useCardedTableFromReactTableWithRef } from '../../components/CardedTableFromReactTable';
import { NativeQueryArgumentNormalized, NativeQueryForm } from '../types';

const columnHelper = createColumnHelper<NativeQueryArgumentNormalized>();

export const ArgumentsField = ({ types }: { types: string[] }) => {
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
            options={types.map(t => ({ label: t, value: t }))}
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
          <BooleanInput name={`arguments.${row.index}.required`} />
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
    [remove, types]
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
                type: 'text',
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
