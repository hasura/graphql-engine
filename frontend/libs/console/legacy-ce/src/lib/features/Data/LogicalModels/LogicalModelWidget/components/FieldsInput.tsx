import { createColumnHelper, useTable } from '@tanstack/react-table';
import { Flex } from '@radix-ui/themes';
import React from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import {
  Button,
  FieldLabel,
  GraphQLSanitizedInputField,
  SwitchField,
  coreTableFeatures,
  CoreTableFeatures,
  createCardedTableFromReactTableWithRef,
  ReactSelect,
  getDialogPortalTarget,
} from '@hasura/shared/ui';
import { LogicalModel } from '@hasura/shared/types';
import {
  AddLogicalModelField,
  AddLogicalModelFormData,
} from '../validationSchema';
import { createFilter } from 'react-select';

const columnHelper = createColumnHelper<
  CoreTableFeatures,
  AddLogicalModelField
>();

const FieldsTableElement =
  createCardedTableFromReactTableWithRef<AddLogicalModelField>();

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
    () =>
      columnHelper.columns([
        columnHelper.accessor('name', {
          id: 'name',
          cell: ({ row }) => (
            <GraphQLSanitizedInputField
              noErrorPlaceholder
              hideTips
              dataTestId={`${name}[${row.index}].name`}
              name={`fields.${row.index}.name`}
              fieldProps={{ placeholder: 'Field Name', disabled }}
            />
          ),
          header: 'Name',
        }),
        columnHelper.accessor('type', {
          id: 'type',
          cell: ({ row }) => {
            const thisField = watch(`fields.${row.index}`);
            return (
              <ReactSelect
                value={{
                  value: `${thisField.typeClass}:${thisField.type}`,
                  label: thisField.type,
                }}
                data-testid={`fields-input-type-${row.index}`}
                onChange={(option) => {
                  if (!option) {
                    return;
                  }

                  const [typeClass, selectedValue] = option?.value.split(':');

                  update(row.index, {
                    ...thisField,
                    type: selectedValue,
                    typeClass: typeClass as any,
                    array: thisField.array,
                  });
                }}
                isDisabled={disabled}
                placeholder="Select a type"
                isSearchable
                filterOption={createFilter({
                  ignoreCase: true,
                  matchFrom: 'any',
                })}
                menuPortalTarget={getDialogPortalTarget()}
                options={[
                  {
                    label: 'Logical Models',
                    options: logicalModels.map((l) => ({
                      label: l.name,
                      value: `logical_model:${l.name}`,
                    })),
                  },
                  {
                    label: 'Types',
                    options: types.map((t) => ({
                      label: t,
                      value: `scalar:${t}`,
                    })),
                  },
                ]}
              />
            );
          },
          header: 'Type',
        }),
        columnHelper.accessor('nullable', {
          id: 'nullable',
          cell: ({ row }) => (
            <SwitchField
              disabled={disabled}
              name={`fields.${row.index}.nullable`}
              noErrorPlaceholder
            />
          ),
          header: 'Nullable',
        }),
        columnHelper.accessor('array', {
          id: 'array',
          header: 'ARRAY',
          cell: ({ row }) => {
            return (
              <SwitchField
                disabled={disabled}
                name={`fields.${row.index}.array`}
                dataTestId={`fields-input-array-${row.index}`}
                noErrorPlaceholder
              />
            );
          },
        }),
        columnHelper.display({
          id: 'action',
          header: 'Actions',
          cell: ({ row }) => (
            <Flex direction="row" gap="2">
              <Button
                disabled={disabled}
                mode="destructive"
                size="1"
                onClick={() => remove(row.index)}
              >
                Remove
              </Button>
            </Flex>
          ),
        }),
      ]),
    [disabled, logicalModels, name, remove, types, update, watch],
  );

  const argumentsTable = useTable({
    features: coreTableFeatures,
    data: fields,
    columns: fieldsColumns,
  });

  return (
    <div>
      <Flex direction="column" gap="2">
        <Flex justify="between" align="center">
          <FieldLabel label="Fields" />
          <Button
            leftIcon={FaPlusCircle}
            disabled={disabled || !types || types.length === 0}
            mode="default"
            size="1"
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
        </Flex>
        <FieldsTableElement
          table={argumentsTable}
          ref={tableRef}
          noRowsMessage={'No fields added'}
        />
      </Flex>
    </div>
  );
};
