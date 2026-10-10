import { useContext, useEffect } from 'react';
import { Flex } from '@radix-ui/themes';
import { Table } from '@hasura/shared/types';
import { tableContext } from './TableProvider';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { getTableDisplayName } from '@hasura/shared/utils';
import { Select } from '@hasura/shared/ui';

export function SelectTable({
  componentLevelId,
  path,
  value,
}: {
  componentLevelId: string;
  path: string[];
  value: Table;
}) {
  const comparatorName = path[path.length - 1];
  const { table, setTable, setComparator } = useContext(tableContext);
  const { setValue } = useContext(rowPermissionsContext);
  const { tables } = useContext(rootTableContext);
  const stringifiedTable = JSON.stringify(table);
  // Sync table name with ColumnsContext table value
  useEffect(() => {
    if (comparatorName === '_table' && !areTablesEqual(value, table)) {
      setTable(value);
    }
  }, [comparatorName, stringifiedTable, value, setTable, setComparator]);

  return (
    <div className="ml-6">
      <Flex gap="4" className="p-2">
        <Select
          data-testid={componentLevelId}
          value={JSON.stringify(value)}
          onChange={(value) => {
            setValue(path, JSON.parse(value) as Table);
          }}
          options={tables.map((t) => {
            const tableDisplayName = getTableDisplayName(t.table);
            return {
              label: tableDisplayName,
              value: JSON.stringify(t.table),
            };
          })}
        />
      </Flex>
    </div>
  );
}
