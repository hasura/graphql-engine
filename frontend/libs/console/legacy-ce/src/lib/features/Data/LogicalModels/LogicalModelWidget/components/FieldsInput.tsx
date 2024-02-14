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
  fieldLabelStyles,
} from '../../../../../new-components/Form';
import { LogicalModel } from '../../../../hasura-metadata-types';
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
  logicalModels,
}: {
  name: string;
  types: string[];
  disabled?: boolean;
  logicalModels: LogicalModel[];
}) => {
  const { control, watch } = useFormContext<AddLogicalModelFormData>();

  const { append, remove, fields, update } = useFieldArray({
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
        cell: ({ row }) => {
          const thisField = watch(`fields.${row.index}`);
          return (
            <select
              className={clsx(
                'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400',
                'text-black',
                disabled
                  ? 'cursor-not-allowed bg-gray-200 border-gray-200 hover:border-gray-200'
                  : 'hover:border-gray-400'
              )}
              value={`${thisField.typeClass}:${thisField.type}`}
              data-testid={`fields-input-type-${row.index}`}
              onChange={e => {
                const [typeClass, selectedValue] = e.target.value.split(':');

                update(row.index, {
                  ...thisField,
                  type: selectedValue,
                  typeClass: typeClass as any,
                  array: thisField.array,
                });
              }}
              disabled={disabled}
            >
              <option value="" data-default-selected hidden>
                Select a type
              </option>
              <optgroup
                label="Logical Models"
                data-testid={`fields-input-type-${row.index}-logical-models`}
              >
                {logicalModels.map(l => (
                  <option key={l.name} value={`logical_model:${l.name}`}>
                    {l.name}
                  </option>
                ))}
              </optgroup>
              <optgroup
                label="Types"
                data-testid={`fields-input-type-${row.index}-scalars`}
              >
                {types.map(t => (
                  <option key={t} value={`scalar:${t}`}>
                    {t}
                  </option>
                ))}
              </optgroup>
            </select>
          );
        },
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
      columnHelper.accessor('array', {
        id: 'array',
        header: 'ARRAY',
        cell: ({ row }) => {
          return (
            <BooleanInput
              disabled={disabled}
              name={`fields.${row.index}.array`}
              dataTestId={`fields-input-array-${row.index}`}
            />
          );
        },
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
    [disabled, logicalModels, name, remove, types, update, watch]
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
            disabled={disabled || !types || types.length === 0}
            onClick={() => {
              append({
                name: '',
                type: 'text',
                typeClass: 'scalar',
                nullable: true,
                array: false,
              });
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
