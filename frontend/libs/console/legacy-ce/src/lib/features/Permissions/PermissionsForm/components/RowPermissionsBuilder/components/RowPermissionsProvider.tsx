import set from 'lodash/set';
import { useCallback, useState, createContext } from 'react';
import { Permissions, RowPermissionsState } from './types';
import { updateKey } from './utils/helpers';
import cloneDeep from 'lodash/cloneDeep';

export const rowPermissionsContext = createContext<RowPermissionsState>({
  comparators: {},
  operators: {},
  permissions: {},
  setValue: () => {
    /* noop */
  },
  setKey: () => {
    /* noop */
  },
  setPermissions: () => {
    /* noop */
  },
});

export const RowPermissionsProvider = ({
  children,
  operators,
  permissions,
  comparators,
  onPermissionsChange,
  loadRelationships,
  isLoading,
}: Pick<
  RowPermissionsState,
  | 'permissions'
  | 'operators'
  | 'comparators'
  | 'loadRelationships'
  | 'isLoading'
> & {
  children?: React.ReactNode | undefined;
  onPermissionsChange?: (permissions: Permissions) => void;
}) => {
  const [permissionsState, setPermissionsState] = useState<
    Pick<RowPermissionsState, 'permissions' | 'operators'>
  >({ operators, permissions });
  const stringifiedPermissions = JSON.stringify(permissionsState.permissions);
  const stringifiedServerPermissionsValue = JSON.stringify(permissions);
  const [prevServerPermissions, setPrevServerPermissions] = useState(
    stringifiedServerPermissionsValue,
  );

  const setValue = useCallback<RowPermissionsState['setValue']>(
    (path, value) => {
      // Clone deep so we don't run into issues with immutability
      // The issue happens when cloning with spread operator, after submitting and getting an error
      // Using cloneDeep we don't run into this issue
      const clone = cloneDeep(permissionsState);
      const newPermissions = set(clone, ['permissions', ...path], value);
      setPermissionsState(newPermissions);
      onPermissionsChange?.(newPermissions.permissions);
    },
    [permissionsState, setPermissionsState, onPermissionsChange],
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
    [permissionsState, setPermissionsState, onPermissionsChange],
  );

  const setPermissions = useCallback<RowPermissionsState['setPermissions']>(
    (permissions) => {
      setPermissionsState({ ...permissionsState, permissions });
      // Set outside permissions when internals change
      onPermissionsChange?.(permissions);
    },
    [permissionsState, setPermissionsState, onPermissionsChange],
  );

  // Set internal state when outside permissions change.
  // This is the React "adjusting state while rendering" pattern: when the
  // server-provided `permissions` prop changes, sync the internal state during
  // render instead of in an effect, which avoids an extra commit/cascading
  // render while preserving identical output.
  if (stringifiedServerPermissionsValue !== prevServerPermissions) {
    setPrevServerPermissions(stringifiedServerPermissionsValue);
    if (stringifiedServerPermissionsValue !== stringifiedPermissions) {
      setPermissionsState((prev) => ({ ...prev, permissions }));
    }
  }
  return (
    <rowPermissionsContext.Provider
      value={{
        ...permissionsState,
        setValue,
        setKey,
        setPermissions,
        comparators,
        loadRelationships,
        isLoading,
      }}
    >
      {children}
    </rowPermissionsContext.Provider>
  );
};
