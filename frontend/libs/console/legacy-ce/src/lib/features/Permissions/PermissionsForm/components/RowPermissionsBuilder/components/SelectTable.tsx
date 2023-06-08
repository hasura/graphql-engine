import { areTablesEqual } from '../../../../../hasura-metadata-api';
import { useContext, useEffect } from 'react';
import { Table } from '../../../../../hasura-metadata-types';
import { getTableDisplayName } from '../../../../../DatabaseRelationships';
import { tableContext } from './TableProvider';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';

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
      <div className="p-2 flex gap-4">
        <select
          data-testid={componentLevelId}
          className="border border-gray-200 rounded-md"
          value={JSON.stringify(value)}
          onChange={e => {
            setValue(path, JSON.parse(e.target.value) as Table);
          }}
        >
          <option value="">-</option>
          {tables.map(t => {
            const tableDisplayName = getTableDisplayName(t.table);
            return (
              // Call JSON.stringify because value cannot be array or object. Will be parsed in setValue
              <option key={tableDisplayName} value={JSON.stringify(t.table)}>
                {tableDisplayName}
              </option>
            );
          })}
        </select>
      </div>
    </div>
  );
}
