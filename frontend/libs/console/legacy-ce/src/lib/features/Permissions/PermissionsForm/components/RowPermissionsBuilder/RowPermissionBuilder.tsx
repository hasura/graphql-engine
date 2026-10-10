import { Metadata, Source, Table } from '@hasura/shared/types';
import { useFormContext } from 'react-hook-form';
import { RowPermissionsInput } from './components';
import { usePermissionTables } from './hooks/usePermissionTables';
import { usePermissionComparators } from './hooks/usePermissionComparators';
import { getNewTablesToLoad } from './utils/relationships';
import { Skeleton } from '@radix-ui/themes';

interface Props {
  permissionsKey: 'check' | 'filter';
  table: Table;
  source: Source;
  metadata: Metadata['metadata'];
}

export const RowPermissionBuilder = ({
  permissionsKey,
  table,
  source,
  metadata,
}: Props) => {
  const { watch, setValue } = useFormContext();

  // by watching the top level of nesting we can get the values for the whole builder
  // this value will always be 'filter' or 'check' depending on the query type

  const value = watch(permissionsKey);
  const { tables, isLoading, tablesToLoad, setTablesToLoad } =
    usePermissionTables({
      source,
      table,
      metadata,
    });

  const comparators = usePermissionComparators();

  return (
    <Skeleton loading={isLoading && !tables}>
      <div
        data-testid="row-permission-builder"
        data-state={JSON.stringify(value)}
      >
        <RowPermissionsInput
          isLoading={isLoading}
          onPermissionsChange={(permissions) => {
            setValue(permissionsKey, permissions);
          }}
          onLoadRelationships={(relationships) => {
            const tablesToAdd = getNewTablesToLoad({
              relationships,
              tablesToLoad,
            });

            if (tablesToAdd.length > 0) {
              setTablesToLoad((previousTablesToLoad) => [
                ...previousTablesToLoad,
                ...tablesToAdd,
              ]);
            }
          }}
          table={table}
          tables={tables ?? []}
          logicalModel={undefined}
          logicalModels={[]}
          permissions={value}
          comparators={comparators}
        />
      </div>
    </Skeleton>
  );
};
