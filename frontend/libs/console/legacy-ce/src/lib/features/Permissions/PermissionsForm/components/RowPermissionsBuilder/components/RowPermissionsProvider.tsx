import set from 'lodash/set';
import { useCallback, useEffect, useState, createContext } from 'react';
import { Permissions, RowPermissionsState } from './types';
import { updateKey } from './utils/helpers';

export const rowPermissionsContext = createContext<RowPermissionsState>({
  comparators: {},
  operators: {},
  permissions: {},
  setValue: () => {},
  setKey: () => {},
  setPermissions: () => {},
});

export const RowPermissionsProvider = ({
  children,
  operators,
  permissions,
  comparators,
  onPermissionsChange,
}: Pick<RowPermissionsState, 'permissions' | 'operators' | 'comparators'> & {
  children?: React.ReactNode | undefined;
  onPermissionsChange?: (permissions: Permissions) => void;
}) => {
  const [permissionsState, setPermissionsState] = useState<
    Pick<RowPermissionsState, 'permissions' | 'operators'>
  >({ operators, permissions });
  const stringifiedPermissions = JSON.stringify(permissionsState.permissions);
  const stringifiedServerPermissionsValue = JSON.stringify(permissions);

  const setValue = useCallback<RowPermissionsState['setValue']>(
    (path, value) => {
      const clone = { ...permissionsState };
      const newPermissions = set(clone, ['permissions', ...path], value);
      setPermissionsState(newPermissions);
      onPermissionsChange?.(newPermissions.permissions);
    },
    [permissionsState, setPermissionsState, onPermissionsChange]
  );
  const setKey = useCallback<RowPermissionsState['setKey']>(
    ({ key, path, type }) => {
      const newPermissions = updateKey({
        permissionsState,
        newKey: key,
        keyPath: path,
        type,
      });
      setPermissionsState(newPermissions);
      onPermissionsChange?.(newPermissions.permissions);
    },
    [permissionsState, setPermissionsState, onPermissionsChange]
  );

  const setPermissions = useCallback<RowPermissionsState['setPermissions']>(
    permissions => {
      setPermissionsState({ ...permissionsState, permissions });
      // Set outside permissions when internals change
      onPermissionsChange?.(permissions);
    },
    [permissionsState, setPermissionsState, onPermissionsChange]
  );

  // Set internal state when outside permissions change
  useEffect(() => {
    if (JSON.stringify(permissions) !== stringifiedPermissions) {
      setPermissionsState({ ...permissionsState, permissions });
    }
  }, [stringifiedServerPermissionsValue, setPermissionsState]);
  return (
    <rowPermissionsContext.Provider
      value={{
        ...permissionsState,
        setValue,
        setKey,
        setPermissions,
        comparators,
      }}
    >
      {children}
    </rowPermissionsContext.Provider>
  );
};
