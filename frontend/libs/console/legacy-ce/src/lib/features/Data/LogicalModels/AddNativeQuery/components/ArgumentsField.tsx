import { createColumnHelper, useTable } from '@tanstack/react-table';
import React, { useRef } from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import {
  Button,
  FieldLabel,
  GraphQLSanitizedInputField,
  InputField,
  SelectField,
  SwitchField,
  coreTableFeatures,
  CoreTableFeatures,
  createCardedTableFromReactTableWithRef,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { NativeQueryArgumentNormalized, NativeQueryForm } from '../types';

const columnHelper = createColumnHelper<
  CoreTableFeatures,
  NativeQueryArgumentNormalized
>();

const ArgumentsTableElement =
  createCardedTableFromReactTableWithRef<NativeQueryArgumentNormalized>();

export const ArgumentsField = ({
  types,
  noSourceSelected,
}: {
  types: string[];
  noSourceSelected: boolean;
}) => {
  const { control } = useFormContext<NativeQueryForm>();

  const { append, remove, fields } = useFieldArray({
    control,
    name: 'arguments',
  });

  const tableRef = useRef<HTMLDivElement>(null);

  const argumentColumns = React.useMemo(
    () =>
      columnHelper.columns([
        columnHelper.accessor('name', {
          id: 'name',
          cell: ({ row }) => (
            <GraphQLSanitizedInputField
              noErrorPlaceholder
              hideTips
              name={`arguments.${row.index}.name`}
              fieldProps={{ placeholder: 'Parameter Name' }}
            />
          ),
          header: 'Name',
        }),
        columnHelper.accessor('type', {
          id: 'type',
          cell: ({ row }) => (
            <SelectField
              // saving prop for future upgrade
              //menuPortalTarget={tableRef.current}
              name={`arguments.${row.index}.type`}
              options={types.map((t) => ({ label: t, value: t }))}
            />
          ),
          header: 'Type',
        }),
        columnHelper.accessor('description', {
          id: 'description',
          cell: ({ row }) => (
            <InputField
              noErrorPlaceholder
              fieldProps={{ placeholder: 'Description' }}
              name={`arguments.${row.index}.description`}
            />
          ),
          header: 'Description',
        }),
        columnHelper.accessor('nullable', {
          id: 'nullable',
          cell: ({ row }) => (
            <SwitchField
              name={`arguments.${row.index}.nullable`}
              dataTestId="nullable-switch"
            />
          ),
          header: 'Nullable',
        }),
        columnHelper.display({
          id: 'action',
          header: 'Actions',
          cell: ({ row }) => (
            <Flex gap="2">
              <Button mode="destructive" onClick={() => remove(row.index)}>
                Remove
              </Button>
            </Flex>
          ),
        }),
      ]),
    [remove, types],
  );

  const argumentsTable = useTable({
    features: coreTableFeatures,
    data: fields,
    columns: argumentColumns,
  });

  return (
    <div className="mb-4">
      <Flex direction="column" gap="2">
        <Flex justify="between" align="center">
          <FieldLabel label="Query Parameters" />
          <Button
            leftIcon={FaPlusCircle}
            disabled={noSourceSelected}
            onClick={() => {
              append({
                name: '',
                type: 'text',
                nullable: false,
                description: '',
              });
            }}
          >
            Add Parameter
          </Button>
        </Flex>
        <ArgumentsTableElement
          table={argumentsTable}
          ref={tableRef}
          noRowsMessage={
            noSourceSelected
              ? 'Select a data source to add query arguments'
              : 'No query parameters added.'
          }
        />
      </Flex>
    </div>
  );
};
