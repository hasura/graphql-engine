import { useState, useContext, useEffect, createContext } from 'react';
import { areTablesEqual } from '@/features/hasura-metadata-api';
import { Table } from '@/features/hasura-metadata-types';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { Columns, Relationships, TableContext } from './types';
import { getTableDisplayName } from '../../../../../DatabaseRelationships/utils/helpers';

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
}: {
  children: React.ReactNode;
  table?: Table;
}) => {
  const [table, setTableName] = useState<Table>(defaultTable || {});
  const [comparator, setComparator] = useState<string | undefined>();
  const [columns, setColumns] = useState<Columns>([]);
  const [relationships, setRelationships] = useState<Relationships>([]);
  const { tables } = useContext(rowPermissionsContext);
  //  Stringify values to get a stable value for useEffect
  const stringifiedTable = JSON.stringify(table);
  const stringifiedTables = JSON.stringify(tables);
  useEffect(() => {
    const foundTable = tables.find(t => areTablesEqual(t.table, table));
    if (foundTable) {
      setColumns(foundTable.columns);
      setRelationships(foundTable.relationships);
    }
  }, [stringifiedTable, stringifiedTables, setColumns, setRelationships]);

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
