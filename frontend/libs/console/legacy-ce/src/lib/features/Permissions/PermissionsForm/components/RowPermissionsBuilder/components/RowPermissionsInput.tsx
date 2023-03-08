import { Table } from '../../../../../hasura-metadata-types';
import { Tables, Operators, Permissions, Comparators } from './types';
import { RowPermissionsProvider } from './RowPermissionsProvider';
import { TypesProvider } from './TypesProvider';
import { TableProvider } from './TableProvider';
import { RootInput } from './RootInput';
import { JsonEditor } from './JsonEditor';

export const RowPermissionsInput = ({
  permissions,
  tables,
  table,
  onPermissionsChange,
  comparators,
}: {
  permissions: Permissions;
  tables: Tables;
  table: Table;
  onPermissionsChange?: (permissions: Permissions) => void;
  comparators: Comparators;
}) => {
  const operators: Operators = {
    boolean: {
      label: 'Bool operators',
      items: [
        { name: '_and', value: '_and' },
        { name: '_not', value: '_not' },
        { name: '_or', value: '_or' },
      ],
    },
    exist: {
      label: 'Exist operators',
      items: [{ name: '_exists', value: '_exists' }],
    },
  };
  return (
    <RowPermissionsProvider
      operators={operators}
      permissions={permissions}
      table={table}
      tables={tables}
      comparators={comparators}
      onPermissionsChange={onPermissionsChange}
    >
      <TypesProvider>
        <TableProvider table={table}>
          <div className="flex flex-col space-y-4 w-full">
            <JsonEditor />
            <RootInput />
          </div>
        </TableProvider>
      </TypesProvider>
    </RowPermissionsProvider>
  );
};
