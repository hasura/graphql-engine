import { TypesContext, PermissionType } from './types';
import {
  useState,
  createContext,
  useCallback,
  useContext,
  useEffect,
} from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import set from 'lodash/set';
import unset from 'lodash/unset';
import { getPermissionTypes } from './utils/typeProviderHelpers';
import { rootTableContext } from './RootTableProvider';

export const typesContext = createContext<TypesContext>({
  types: {},
  setType: () => {},
});

// Provides types for permissions
// This is used to determine if a permission is a column or relationship or nested object
export const TypesProvider = ({ children }: { children: React.ReactNode }) => {
  const [types, setTypes] = useState<Record<string, { type: PermissionType }>>(
    {}
  );
  const { permissions } = useContext(rowPermissionsContext);
  const { table, tables } = useContext(rootTableContext);
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
      setTypes(prev => {
        // Remove old path
        const newTypes = { ...prev };
        unset(newTypes, path.join('.'));
        // Set new type on new path
        set(newTypes, [...path.slice(0, -1), value].join('.'), { type });

        return newTypes;
      });
    },
    []
  );
  //  Stringify values to get a stable value for useEffect
  const jsonPermissions = JSON.stringify(permissions);
  const stringifiedTable = JSON.stringify(table);
  const stringifiedTables = JSON.stringify(tables);
  // Recursively set types
  useEffect(() => {
    const newTypes = getPermissionTypes(tables, table, permissions);

    setTypes(newTypes);
  }, [jsonPermissions, setTypes, stringifiedTable, stringifiedTables]);

  return (
    <typesContext.Provider value={{ types, setType }}>
      {children}
    </typesContext.Provider>
  );
};
