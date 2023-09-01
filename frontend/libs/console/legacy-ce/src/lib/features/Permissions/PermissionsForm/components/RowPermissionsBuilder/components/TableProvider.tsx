import { useState, useContext, useEffect, createContext } from 'react';
import { Table } from '../../../../../hasura-metadata-types';
import { Columns, Relationships, TableContext } from './types';
import { rootTableContext } from './RootTableProvider';
import { areTablesEqual } from '../../../../../hasura-metadata-api';
import { fieldsToColumns } from './utils/nestedObjects';
import { rowPermissionsContext } from './RowPermissionsProvider';

export const tableContext = createContext<TableContext>({
  table: {},
  setTable: () => {},
  comparator: undefined,
  setComparator: () => {},
  columns: [],
  setColumns: () => {},
  relationships: [],
  setRelationships: () => {},
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
  const [table, setTableName] = useState<Table>(defaultTable || {});
  const [comparator, setComparator] = useState<string | undefined>();
  const [columns, setColumns] = useState<Columns>([]);
  const [relationships, setRelationships] = useState<Relationships>([]);
  const { tables, rootTable } = useContext(rootTableContext);
  const { loadRelationships } = useContext(rowPermissionsContext);
  const { table: closestTableName } = useContext(tableContext);
  const closestTable = tables.find(t =>
    areTablesEqual(t.table, closestTableName)
  );

  const supportedSources = tables
    .map(i => i.dataSource)
    .filter(s => s?.kind === 'postgres')
    .map(s => s?.name);

  //  Stringify values to get a stable value for useEffect
  const stringifiedTable = JSON.stringify(table);
  const stringifiedTables = JSON.stringify(tables);

  useEffect(() => {
    const foundTable = tables.find(t => areTablesEqual(t.table, table));
    if (foundTable) {
      setColumns(foundTable.columns);
      if (foundTable?.dataSource?.name !== rootTable?.dataSource?.name) return;
      setRelationships(
        foundTable.relationships.filter(rel => {
          return (
            rel.type === 'localRelationship' ||
            (rel.type !== 'remoteSchemaRelationship' &&
              supportedSources?.includes(rel.definition.toSource))
          );
        })
      );
      // Load initial related tables' columns
      loadRelationships?.(foundTable.relationships);
    }
    // If it's a nested object, set its fields as columns instead of the parent's columns
    if (objectPath) {
      setColumns(
        fieldsToColumns(
          closestTable?.columns?.find(c => {
            const objectPathArray = objectPath.split('.');
            const path = objectPathArray[objectPathArray.length - 1];
            return c.name === path;
          })
        )
      );
    }
  }, [
    stringifiedTable,
    stringifiedTables,
    setColumns,
    setRelationships,
    objectPath,
    loadRelationships,
  ]);

  return (
    <tableContext.Provider
      value={{
        columns,
        setColumns,
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
