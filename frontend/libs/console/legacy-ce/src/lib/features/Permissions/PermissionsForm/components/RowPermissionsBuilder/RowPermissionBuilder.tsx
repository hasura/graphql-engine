import { Table } from '@/features/hasura-metadata-types';

import { useFormContext } from 'react-hook-form';
import { RowPermissionsInput } from './components';
import { usePermissionTables } from './hooks/usePermissionTables';
import { usePermissionComparators } from './hooks/usePermissionComparators';
import { useMetadataTable } from '../../../../hasura-metadata-api/metadataHooks';
import { getMetadataTableCustomName } from './utils/getMetadataTableCustomName';

interface Props {
  nesting: string[];
  table: Table;
  dataSourceName: string;
}

export const RowPermissionBuilder = ({
  nesting,
  table,
  dataSourceName,
}: Props) => {
  const { watch, setValue } = useFormContext();

  // by watching the top level of nesting we can get the values for the whole builder
  // this value will always be 'filter' or 'check' depending on the query type
  const permissionsKey = nesting[0];

  const value = watch(permissionsKey);

  const { data: tableConfig } = useMetadataTable(dataSourceName, table);
  const tables = usePermissionTables({
    dataSourceName,
    tableCustomName: getMetadataTableCustomName(tableConfig),
  });

  const comparators = usePermissionComparators();

  if (!tables) return <>Loading</>;
  return (
    <RowPermissionsInput
      onPermissionsChange={permissions => {
        setValue(permissionsKey, permissions);
      }}
      table={table}
      tables={tables}
      permissions={value}
      comparators={comparators}
    />
  );
};
