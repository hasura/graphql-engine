import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import clsx from 'clsx';
import React from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import {
  GraphQLSanitizedInputField,
  Select,
  fieldLabelStyles,
} from '../../../../../new-components/Form';
import { BooleanInput } from '../../components/BooleanInput';
import { useCardedTableFromReactTableWithRef } from '../../components/CardedTableFromReactTable';
import {
  AddLogicalModelField,
  AddLogicalModelFormData,
} from '../validationSchema';

const columnHelper = createColumnHelper<AddLogicalModelField>();

export const FieldsInput = ({
  name,
  types,
  disabled,
}: {
  name: string;
  types: string[];
  disabled?: boolean;
}) => {
  const { control } = useFormContext<AddLogicalModelFormData>();

  const { append, remove, fields } = useFieldArray({
    control,
    name: 'fields',
  });

  const tableRef = React.useRef<HTMLDivElement>(null);

  const fieldsColumns = React.useMemo(
    () => [
      columnHelper.accessor('name', {
        id: 'name',
        cell: ({ row }) => (
          <GraphQLSanitizedInputField
            noErrorPlaceholder
            hideTips
            dataTestId={`${name}[${row.index}].name`}
            placeholder="Field Name"
            name={`fields.${row.index}.name`}
            disabled={disabled}
          />
        ),
        header: 'Name',
      }),
      columnHelper.accessor('type', {
        id: 'type',
        cell: ({ row }) => (
          <Select
            noErrorPlaceholder
            dataTestId={`${name}[${row.index}].type`}
            name={`fields.${row.index}.type`}
            options={types.map(t => ({ label: t, value: t }))}
            disabled={disabled}
          />
        ),
        header: 'Type',
      }),

      columnHelper.accessor('nullable', {
        id: 'nullable',
        cell: ({ row }) => (
          <BooleanInput
            disabled={disabled}
            name={`fields.${row.index}.nullable`}
          />
        ),
        header: 'Nullable',
      }),
      columnHelper.display({
        id: 'action',
        header: 'Actions',
        cell: ({ row }) => (
          <div className="flex flex-row gap-2">
            <Button
              disabled={disabled}
              mode="destructive"
              onClick={() => remove(row.index)}
            >
              Remove
            </Button>
          </div>
        ),
      }),
    ],
    [disabled, name, remove, types]
  );

  const argumentsTable = useReactTable({
    data: fields,
    columns: fieldsColumns,
    getCoreRowModel: getCoreRowModel(),
  });

  const FieldsTableElement =
    useCardedTableFromReactTableWithRef<AddLogicalModelField>();

  return (
    <div>
      <div className="flex flex-col gap-2 ">
        <div className="flex justify-between items-center">
          <div className={clsx(fieldLabelStyles, 'mb-0')}>Fields</div>
          <Button
            icon={<FaPlusCircle />}
            disabled={!types || types.length === 0}
            onClick={() => {
              append({ name: '', type: 'text', nullable: true });
            }}
          >
            Add Field
          </Button>
        </div>
        <FieldsTableElement
          table={argumentsTable}
          ref={tableRef}
          noRowsMessage={'No fields added'}
        />
      </div>
    </div>
  );
};
