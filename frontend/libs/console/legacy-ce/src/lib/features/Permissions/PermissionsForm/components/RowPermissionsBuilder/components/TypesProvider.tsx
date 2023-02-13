import { TypesContext, PermissionType } from './types';
import {
  useState,
  createContext,
  useCallback,
  useContext,
  useEffect,
} from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { set, unset } from 'lodash';
import { getPermissionTypes } from './utils/typeProviderHelpers';

export const typesContext = createContext<TypesContext>({
  types: {},
  setType: () => {},
});

// Provides types for permissions
// This is used to determine if a permission is a column or relationship
export const TypesProvider = ({ children }: { children: React.ReactNode }) => {
  const [types, setTypes] = useState<Record<string, { type: PermissionType }>>(
    {}
  );
  const { permissions, tables, table } = useContext(rowPermissionsContext);
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
  // Recursively set types
  useEffect(() => {
    const newTypes = getPermissionTypes(tables, table, permissions);

    setTypes(newTypes);
  }, [jsonPermissions, setTypes, stringifiedTable]);

  return (
    <typesContext.Provider value={{ types, setType }}>
      {children}
    </typesContext.Provider>
  );
};
