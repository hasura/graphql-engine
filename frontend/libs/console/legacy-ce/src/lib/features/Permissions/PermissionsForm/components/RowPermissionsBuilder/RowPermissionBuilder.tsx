import { Table } from '../../../../hasura-metadata-types';

import { useFormContext } from 'react-hook-form';
import { RowPermissionsInput } from './components';
import { usePermissionTables } from './hooks/usePermissionTables';
import { usePermissionComparators } from './hooks/usePermissionComparators';
import Skeleton from 'react-loading-skeleton';

interface Props {
  permissionsKey: 'check' | 'filter';
  table: Table;
  dataSourceName: string;
}

export const RowPermissionBuilder = ({
  permissionsKey,
  table,
  dataSourceName,
}: Props) => {
  const { watch, setValue } = useFormContext();

  // by watching the top level of nesting we can get the values for the whole builder
  // this value will always be 'filter' or 'check' depending on the query type

  const value = watch(permissionsKey);

  const { tables, isLoading } = usePermissionTables({
    dataSourceName,
  });

  const comparators = usePermissionComparators();

  if (isLoading || !tables) return <Skeleton />;
  return (
    <div
      data-testid="row-permission-builder"
      data-state={JSON.stringify(value)}
    >
      <RowPermissionsInput
        onPermissionsChange={permissions => {
          setValue(permissionsKey, permissions);
        }}
        table={table}
        tables={tables}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={value}
        comparators={comparators}
      />
    </div>
  );
};
