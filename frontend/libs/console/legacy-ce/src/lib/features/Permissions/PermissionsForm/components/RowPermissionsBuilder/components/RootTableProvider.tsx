import { createContext } from 'react';
import { Table } from '@hasura/shared/types';
import { Tables } from './types';
import { areTablesEqual } from '@hasura/metadata/helpers';

type RootTableState = {
  tables: Tables;
  table: Table | undefined;
  rootTable: Tables[number] | undefined;
};

export const rootTableContext = createContext<RootTableState>({
  table: [],
  tables: [],
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
}: Omit<RootTableState, 'rootTable'> & {
  children?: React.ReactNode | undefined;
}) => {
  const rootTable = table
    ? tables.find((t) => areTablesEqual(t.table, table))
    : undefined;
  return (
    <rootTableContext.Provider
      value={{
        table,
        tables,
        rootTable,
      }}
    >
      {children}
    </rootTableContext.Provider>
  );
};
