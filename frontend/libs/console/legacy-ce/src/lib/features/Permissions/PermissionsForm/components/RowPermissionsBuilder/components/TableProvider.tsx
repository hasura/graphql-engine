import { useState, useContext, useEffect, createContext } from 'react';
import { ComputedField, Table } from '@hasura/shared/types';
import { Columns, Relationships, TableContext } from './types';
import { rootTableContext } from './RootTableProvider';
import { fieldsToColumns } from './utils/nestedObjects';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { getTableDisplayName } from '@hasura/shared/utils';

export const tableContext = createContext<TableContext>({
  table: [],
  setTable: () => {
    /* noop */
  },
  comparator: undefined,
  setComparator: () => {
    /* noop */
  },
  columns: [],
  setColumns: () => {
    /* noop */
  },
  computedFields: [],
  setComputedFields: () => {
    /* noop */
  },
  relationships: [],
  setRelationships: () => {
    /* noop */
  },
});

export const TableProvider = ({
  children,
  table: defaultTable,
  objectPath,
}: {
  children: React.ReactNode;
  table?: Table;
  objectPath?: string;
}) => {
  const [table, setTableName] = useState<Table>(defaultTable || ({} as Table));
  const [comparator, setComparator] = useState<string | undefined>();
  const [columns, setColumns] = useState<Columns>([]);
  const [computedFields, setComputedFields] = useState<ComputedField[]>([]);
  const [relationships, setRelationships] = useState<Relationships>([]);
  const { tables, rootTable } = useContext(rootTableContext);
  const { loadRelationships } = useContext(rowPermissionsContext);
  const { table: closestTableName } = useContext(tableContext);
  const closestTable = tables.find((t) =>
    areTablesEqual(t.table, closestTableName),
  );

  const supportedSources = tables
    .map((i) => i.dataSource)
    .filter((s) => s?.kind === 'postgres')
    .map((s) => s?.name);

  //  Stringify values to get a stable key for the derived-state sync below
  const stringifiedTable = getTableDisplayName(table);
  const stringifiedTables = `[${tables.map((t) => getTableDisplayName(t)).join(',')}]`;
  const syncKey = `${stringifiedTable}|${stringifiedTables}|${
    objectPath ?? ''
  }|${closestTableName ? getTableDisplayName(closestTableName) : ''}|${
    rootTable?.dataSource?.name ?? ''
  }`;

  // Sync the derived columns/computed fields/relationships while rendering when
  // the relevant inputs change. This is the React "adjusting state during
  // render" pattern: it avoids the cascading renders of setting this state in an
  // effect while producing identical output (the setters are idempotent, so
  // unchanged values bail out). `loadRelationships` is a genuine side effect and
  // is kept in the effect below.
  const [prevSyncKey, setPrevSyncKey] = useState<string | null>(null);
  if (syncKey !== prevSyncKey) {
    setPrevSyncKey(syncKey);
    const foundTable = tables.find((t) => areTablesEqual(t.table, table));
    let skipObjectPath = false;
    if (foundTable) {
      setColumns(foundTable.columns);
      setComputedFields(foundTable.computedFields);
      if (foundTable?.dataSource?.name !== rootTable?.dataSource?.name) {
        skipObjectPath = true;
      } else {
        setRelationships(
          foundTable.relationships.filter((rel) => {
            return (
              rel.type === 'localRelationship' ||
              (rel.type !== 'remoteSchemaRelationship' &&
                supportedSources?.includes(rel.definition.toSource))
            );
          }),
        );
      }
    }
    // If it's a nested object, set its fields as columns instead of the parent's columns
    if (!skipObjectPath && objectPath) {
      setColumns(
        fieldsToColumns(
          closestTable?.columns?.find((c) => {
            const objectPathArray = objectPath.split('.');
            const path = objectPathArray[objectPathArray.length - 1];
            return c.name === path;
          }),
        ),
      );
    }
  }

  useEffect(() => {
    const foundTable = tables.find((t) => areTablesEqual(t.table, table));
    if (
      foundTable &&
      foundTable?.dataSource?.name === rootTable?.dataSource?.name
    ) {
      // Load initial related tables' columns
      loadRelationships?.(foundTable.relationships);
    }
  }, [table, tables, rootTable?.dataSource?.name, loadRelationships]);

  return (
    <tableContext.Provider
      value={{
        columns,
        setColumns,
        computedFields,
        setComputedFields,
        table,
        setTable: setTableName,
        relationships,
        setRelationships,
        comparator,
        setComparator,
      }}
    >
      {children}
    </tableContext.Provider>
  );
};
