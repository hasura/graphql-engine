import { createColumnHelper, useTable } from '@tanstack/react-table';
import React, { useRef } from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { GDCFormSchema } from '../useFormValidationSchema';
import { FaPlusCircle } from 'react-icons/fa';
import {
  Button,
  GraphQLSanitizedInputField,
  InputField,
  IconTooltip,
  coreTableFeatures,
  CoreTableFeatures,
} from '@hasura/shared/ui';
import { createCardedTableFromReactTableWithRef } from '../../../../Data';
import { Flex } from '@radix-ui/themes';

type Variable = {
  name: string;
  type: string;
  filepath: string;
};
const columnHelper = createColumnHelper<CoreTableFeatures, Variable>();
const TemplateVariablesTableElement =
  createCardedTableFromReactTableWithRef<Variable>();

export const TemplateVariables = () => {
  const { control } = useFormContext<GDCFormSchema>();

  const { append, remove, fields } = useFieldArray({
    control,
    name: 'template_variables',
  });

  const tableRef = useRef<HTMLDivElement>(null);

  const columns = React.useMemo(
    () => [
      columnHelper.accessor('name', {
        id: 'name',
        cell: ({ row }) => (
          <GraphQLSanitizedInputField
            noErrorPlaceholder
            hideTips
            name={`template_variables.${row.index}.name`}
            fieldProps={{
              placeholder: 'Variable Name',
            }}
          />
        ),
        header: 'Name',
      }),
      // for now there's only 1 option ever and it's `dynamic_from_file`
      // this is being set when a variable append() is called
      // columnHelper.accessor('type', {
      //   id: 'type',

      //   cell: ({ row }) => (
      //     <Select
      //       noErrorPlaceholder
      //       name={`template_variables.${row.index}.type`}
      //       options={['dynamic_from_file'].map(t => ({ label: t, value: t }))}
      //     />
      //   ),
      //   header: 'Type',
      // }),
      columnHelper.accessor('filepath', {
        id: 'description',
        cell: ({ row }) => (
          <InputField
            noErrorPlaceholder
            name={`template_variables.${row.index}.filepath`}
            fieldProps={{
              placeholder: 'File Path',
            }}
          />
        ),
        header: () => {
          const toolTipMessage = (
            <Flex direction="column" gap="3">
              <p>
                Specify a file system path to dynamically load the variable
                value.
              </p>
              <p>
                This feature requires the environment variable:{' '}
                <pre>HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX</pre> to
                be set to the path that you will locate your files in. For
                example, you could use: <pre>/var/secrets/</pre>
              </p>
              <p>
                Only file paths that have this prefix will be allowed to be
                accessed as a template variable.
              </p>
            </Flex>
          );
          return (
            <Flex align="center">
              File Path
              <IconTooltip message={toolTipMessage} />
            </Flex>
          );
        },
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
    ],
    [remove],
  );

  const table = useTable({
    features: coreTableFeatures,
    data: fields,
    columns: columns,
  } as any);

  return (
    <div>
      <Flex direction="column" gap="2">
        <Flex justify="between" align="center">
          <div className={'text-gray-600 font-semibold'}>
            Template Variables
          </div>
          <Button
            leftIcon={FaPlusCircle}
            onClick={() => {
              append({
                name: '',
                type: 'dynamic_from_file',
                filepath: '',
              });
            }}
          >
            Add Variable
          </Button>
        </Flex>
        <TemplateVariablesTableElement
          table={table as any}
          ref={tableRef}
          noRowsMessage={'No template variables added.'}
        />
      </Flex>
    </div>
  );
};
