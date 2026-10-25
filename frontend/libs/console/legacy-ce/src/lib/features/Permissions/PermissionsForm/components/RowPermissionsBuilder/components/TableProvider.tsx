import { useState, useContext, useEffect, createContext } from 'react';
import { ComputedField, Table } from '@hasura/shared/types';
import { Columns, Relationships, TableContext } from './types';
import { rootTableContext } from './RootTableProvider';
import { fieldsToColumns } from './utils/nestedObjects';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { getTableDisplayName } from '@hasura/shared/utils';
import { findTable } from './utils/tables';

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
  source: sourceProp,
  objectPath,
}: {
  children: React.ReactNode;
  table?: Table;
  /** the source of the table, the one of the closest table by default */
  source?: string;
  objectPath?: string;
}) => {
  const [table, setTableName] = useState<Table>(defaultTable || ({} as Table));
  const [comparator, setComparator] = useState<string | undefined>();
  const [columns, setColumns] = useState<Columns>([]);
  const [computedFields, setComputedFields] = useState<ComputedField[]>([]);
  const [relationships, setRelationships] = useState<Relationships>([]);
  const {
    tables,
    rootTable,
    source: rootSource,
  } = useContext(rootTableContext);
  const { loadRelationships } = useContext(rowPermissionsContext);
  const { table: closestTableName, source: closestSource } =
    useContext(tableContext);
  const closestTable = findTable(tables, closestTableName, closestSource);
  const source = sourceProp ?? closestSource ?? rootSource;

  const supportedSources = tables
    .map((i) => i.dataSource)
    .filter((s) => s?.kind === 'postgres')
    .map((s) => s?.name);

  //  Stringify values to get a stable key for the derived-state sync below
  const stringifiedTable = getTableDisplayName(table);
  const stringifiedTables = `[${tables.map((t) => getTableDisplayName(t)).join(',')}]`;
  const syncKey = `${stringifiedTable}|${source ?? ''}|${stringifiedTables}|${
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
    const foundTable = findTable(tables, table, source);
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
    const foundTable = findTable(tables, table, source);
    if (
      foundTable &&
      foundTable?.dataSource?.name === rootTable?.dataSource?.name
    ) {
      // Load initial related tables' columns
      loadRelationships?.(foundTable.relationships);
    }
  }, [table, source, tables, rootTable?.dataSource?.name, loadRelationships]);

  return (
    <tableContext.Provider
      value={{
        source,
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
