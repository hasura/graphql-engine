import { LogicalModel, Table } from '../../../../../hasura-metadata-types';
import {
  Tables,
  Operators,
  Permissions,
  Comparators,
  Relationships,
} from './types';
import { RowPermissionsProvider } from './RowPermissionsProvider';
import { TypesProvider } from './TypesProvider';
import { TableProvider } from './TableProvider';
import { RootInput } from './RootInput';
import { JsonEditor } from './JsonEditor';
import { RootTableProvider } from './RootTableProvider';
import { RootLogicalModelProvider } from './RootLogicalModelProvider';
import {
  ForbiddenFeaturesProvider,
  Feature,
} from './ForbiddenFeaturesProvider';
import { LogicalModelWithSourceName } from '../../../../LogicalModelPermissions/components/types';

export const RowPermissionsInput = ({
  permissions,
  tables,
  table,
  logicalModel,
  logicalModels,
  onPermissionsChange,
  onLoadRelationships,
  comparators,
  forbidden,
  isLoading,
}: {
  permissions: Permissions;
  tables: Tables;
  table: Table | undefined;
  logicalModels: LogicalModelWithSourceName[];
  logicalModel: LogicalModel['name'] | undefined;
  onPermissionsChange?: (permissions: Permissions) => void;
  onLoadRelationships?: (relationships: Relationships) => void;
  comparators: Comparators;
  forbidden?: Feature[];
  isLoading?: boolean;
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
    <ForbiddenFeaturesProvider forbidden={forbidden}>
      <RootLogicalModelProvider
        logicalModel={logicalModel}
        logicalModels={logicalModels}
      >
        <RootTableProvider table={table} tables={tables}>
          <RowPermissionsProvider
            operators={operators}
            permissions={permissions}
            comparators={comparators}
            onPermissionsChange={onPermissionsChange}
            loadRelationships={onLoadRelationships}
            isLoading={isLoading}
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
        </RootTableProvider>
      </RootLogicalModelProvider>
    </ForbiddenFeaturesProvider>
  );
};
