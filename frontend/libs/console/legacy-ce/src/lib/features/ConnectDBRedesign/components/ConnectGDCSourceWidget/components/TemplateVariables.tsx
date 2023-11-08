import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React, { useRef } from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import {
  GraphQLSanitizedInputField,
  InputField,
} from '../../../../../new-components/Form';
import { GDCFormSchema } from '../useFormValidationSchema';

import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { useCardedTableFromReactTableWithRef } from '../../../../Data';
type Variable = {
  name: string;
  type: string;
  filepath: string;
};
const columnHelper = createColumnHelper<Variable>();

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
            placeholder="Variable Name"
            name={`template_variables.${row.index}.name`}
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
            placeholder="File Path"
            name={`template_variables.${row.index}.filepath`}
          />
        ),
        header: () => {
          const toolTipMessage = (
            <div className="flex flex-col gap-3">
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
            </div>
          );
          return (
            <div className="flex items-center">
              File Path
              <IconTooltip message={toolTipMessage} />
            </div>
          );
        },
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
    [remove]
  );

  const table = useReactTable({
    data: fields,
    columns: columns,
    getCoreRowModel: getCoreRowModel(),
  });

  const TableElement = useCardedTableFromReactTableWithRef<Variable>();

  return (
    <div>
      <div className="flex flex-col gap-2 ">
        <div className="flex justify-between items-center">
          <div className={'text-gray-600 font-semibold'}>
            Template Variables
          </div>
          <Button
            icon={<FaPlusCircle />}
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
        </div>
        <TableElement
          table={table}
          ref={tableRef}
          noRowsMessage={'No template variables added.'}
        />
      </div>
    </div>
  );
};
