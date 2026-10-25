import { TypesContext, PermissionType } from './types';
import { useState, createContext, useCallback, useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import set from 'lodash/set';
import unset from 'lodash/unset';
import { getPermissionTypes } from './utils/typeProviderHelpers';
import { rootTableContext } from './RootTableProvider';
import { getTableDisplayName } from '@hasura/shared/utils';

export const typesContext = createContext<TypesContext>({
  types: {},
  setType: () => {
    /* noop */
  },
});

// Provides types for permissions
// This is used to determine if a permission is a column or relationship or nested object
export const TypesProvider = ({ children }: { children: React.ReactNode }) => {
  const [types, setTypes] = useState<Record<string, { type: PermissionType }>>(
    {},
  );
  const { permissions } = useContext(rowPermissionsContext);
  const { table, tables, source } = useContext(rootTableContext);
  const setType = useCallback(
    ({
      type,
      path,
      value,
    }: {
      type: PermissionType;
      path: string[];
      value: any;
    }) => {
      setTypes((prev) => {
        // Remove old path
        const newTypes = { ...prev };
        unset(newTypes, path.join('.'));
        // Set new type on new path
        set(newTypes, [...path.slice(0, -1), value].join('.'), { type });

        return newTypes;
      });
    },
    [],
  );
  //  Stringify values to get a stable key for the derived-state sync below
  const jsonPermissions = JSON.stringify(permissions);
  const stringifiedTable = table ? getTableDisplayName(table) : '';
  const stringifiedTables = `[${tables.map((t) => getTableDisplayName(t)).join(',')}]`;
  const typesKey = `${jsonPermissions}|${stringifiedTable}|${source ?? ''}|${stringifiedTables}`;
  // Recursively (re)compute types while rendering when permissions/table/tables
  // change. This is the React "adjusting state during render" pattern: it fully
  // recomputes types from the inputs (as the effect did) without the cascading
  // renders of setting state in an effect, while `setType` still supports
  // incremental updates.
  const [prevTypesKey, setPrevTypesKey] = useState<string | null>(null);
  if (typesKey !== prevTypesKey) {
    setPrevTypesKey(typesKey);
    setTypes(getPermissionTypes(tables, table, permissions, source));
  }

  return (
    <typesContext.Provider value={{ types, setType }}>
      {children}
    </typesContext.Provider>
  );
};
