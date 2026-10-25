import { Metadata, Source, Table } from '@hasura/shared/types';
import { useFormContext, useWatch } from 'react-hook-form';
import { useCallback } from 'react';
import { Permissions, Relationships, RowPermissionsInput } from './components';
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
  const { setValue } = useFormContext();

  // by watching the top level of nesting we can get the values for the whole builder
  // this value will always be 'filter' or 'check' depending on the query type.
  // `useWatch` re-renders only the builder on a change, whereas `watch` would
  // re-render the whole permissions form on every keystroke
  const value = useWatch({ name: permissionsKey });
  const { tables, isLoading, setTablesToLoad } = usePermissionTables({
    source,
    table,
    metadata,
  });

  const comparators = usePermissionComparators();

  // stable, as every table of the builder loads the tables of its
  // relationships whenever this changes
  const onLoadRelationships = useCallback(
    (relationships: Relationships) =>
      setTablesToLoad((previousTablesToLoad) => {
        const tablesToAdd = getNewTablesToLoad({
          relationships,
          tablesToLoad: previousTablesToLoad,
        });

        return tablesToAdd.length > 0
          ? [...previousTablesToLoad, ...tablesToAdd]
          : previousTablesToLoad;
      }),
    [setTablesToLoad],
  );

  const onPermissionsChange = useCallback(
    (permissions: Permissions) => setValue(permissionsKey, permissions),
    [setValue, permissionsKey],
  );

  return (
    <Skeleton loading={isLoading && !tables}>
      <div
        data-testid="row-permission-builder"
        data-state={JSON.stringify(value)}
      >
        <RowPermissionsInput
          isLoading={isLoading}
          onPermissionsChange={onPermissionsChange}
          onLoadRelationships={onLoadRelationships}
          table={table}
          source={source.name}
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
