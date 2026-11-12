import { createContext, useMemo } from 'react';
import { Table } from '@hasura/shared/types';
import { Tables } from './types';
import { findTable } from './utils/tables';

const tablesIds = new WeakMap<Tables, number>();
let nextTablesId = 0;

/**
 * A key per tables array: the tables are a new array whenever they change
 * (e.g. the columns of a table loaded), and serializing them instead would
 * mean serializing every table of the metadata with its columns on every
 * render of every TableProvider.
 */
export const getTablesKey = (tables: Tables) => {
  let id = tablesIds.get(tables);
  if (id === undefined) {
    id = nextTablesId++;
    tablesIds.set(tables, id);
  }
  return String(id);
};

type RootTableState = {
  tables: Tables;
  /** a key that changes when the tables change */
  tablesKey: string;
  table: Table | undefined;
  /** the source of the root table */
  source?: string;
  rootTable: Tables[number] | undefined;
};

export const rootTableContext = createContext<RootTableState>({
  table: [],
  tables: [],
  tablesKey: '[]',
  rootTable: undefined,
});

/**
 * RootTableProvider
 *
 * Provides tables array, table definition (just name) and root table (full table) in context so it can be accessed in any of its children
 * Not to be confused with TableProvider, which provides the current table
 * RootTableProvider is meant to be used just once, at the top of the tree,
 * whereas there are many TableProvider in the tree, used mainly for selecting relationships
 */
export const RootTableProvider = ({
  children,
  table,
  tables,
  source,
}: Omit<RootTableState, 'rootTable' | 'tablesKey'> & {
  children?: React.ReactNode | undefined;
}) => {
  const value = useMemo(() => {
    const rootTable = table ? findTable(tables, table, source) : undefined;
    return {
      table,
      tables,
      tablesKey: getTablesKey(tables),
      source: source ?? rootTable?.dataSource?.name,
      rootTable,
    };
  }, [table, tables, source]);

  return (
    <rootTableContext.Provider value={value}>
      {children}
    </rootTableContext.Provider>
  );
};
