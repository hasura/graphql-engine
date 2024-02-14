import { Table } from '../../../../hasura-metadata-types';

import { useFormContext } from 'react-hook-form';
import { RowPermissionsInput, TableToLoad } from './components';
import { usePermissionTables } from './hooks/usePermissionTables';
import { usePermissionComparators } from './hooks/usePermissionComparators';
import Skeleton from 'react-loading-skeleton';
import { useState } from 'react';
import { getNewTablesToLoad } from './utils/relationships';

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
  const [tablesToLoad, setTablesToLoad] = useState<TableToLoad>([
    { table, source: dataSourceName },
  ]);

  const { tables, isLoading } = usePermissionTables({
    dataSourceName,
    tablesToLoad,
  });

  const comparators = usePermissionComparators();

  if (!tables) return <Skeleton />;
  return (
    <div
      data-testid="row-permission-builder"
      data-state={JSON.stringify(value)}
    >
      <RowPermissionsInput
        isLoading={isLoading}
        onPermissionsChange={permissions => {
          setValue(permissionsKey, permissions);
        }}
        onLoadRelationships={relationships => {
          const tablesToAdd = getNewTablesToLoad({
            relationships,
            tablesToLoad,
          });

          if (tablesToAdd.length > 0) {
            setTablesToLoad(previousTablesToLoad => [
              ...previousTablesToLoad,
              ...tablesToAdd,
            ]);
          }
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
