import get from 'lodash/get';
import isEmpty from 'lodash/isEmpty';
import set from 'lodash/set';
import unset from 'lodash/unset';
import isObjectLike from 'lodash/isObjectLike';
import { RowPermissionsState, PermissionType } from '../types';
import { allOperators } from './comparatorsFromSchema';
import cloneDeep from 'lodash/cloneDeep';

const getKeyPath = ({
  keyPath,
  newKey,
  permissionsState,
  type,
}: {
  permissionsState: Pick<RowPermissionsState, 'permissions' | 'operators'>;
  keyPath: string[];
  newKey: string;
  type?: PermissionType;
}) => {
  // Store value before deleting key
  const value = get(permissionsState, ['permissions', ...keyPath]);
  let path = keyPath;

  const isNestedComparator =
    isComparator(newKey) &&
    newKey !== '_exists' && // ignore _exists which is a special comparator
    path.length >= 1;

  const previousKey = keyPath[keyPath.length - 1];
  if (
    // Replacing an `_and` comparator that's empty (as opposed to the default `{}`) with a column key
    isEmptyArray(value, previousKey) ||
    !isEmpty(value) ||
    isNestedComparator
  ) {
    path = replacePath(keyPath, permissionsState);
  }

  if ((previousKey === '_not' && newKey === '_and') || newKey === '_or') {
    path = replacePath(keyPath, permissionsState);
  }

  if (newKey === '') return ['permissions', ...path];

  return appendToPath(path, newKey);
};

function replacePath(
  keyPath: string[],
  permissionsState: Pick<RowPermissionsState, 'permissions' | 'operators'>
) {
  unset(permissionsState, ['permissions', ...keyPath]);
  return keyPath.slice(0, -1);
}

function appendToPath(path: string[], newKey: string) {
  return ['permissions', ...path, newKey];
}

const getInitialValue = (key: string, type?: PermissionType) => {
  switch (key) {
    case '_and':
      return [{}];
    case '_or':
      return [{}];
    case '_not':
    case '_contains':
    case '_contained_in':
    case '_st_d_within':
    case '_st_within':
    case '_st_3d_d_within':
    case '_st_contains':
    case '_st_crosses':
    case '_st_intersects':
    case '_st_touches':
    case '_st_overlaps':
      return {};
    case '_is_null':
      return false;
    case '_exists':
      return {
        _where: {},
        _table: {},
      };
    case '_nin':
    case '_in':
    case '_has_keys_all':
    case '_has_keys_any':
      return [''];
  }

  switch (type) {
    case 'column':
      // Depends on column type
      return { _eq: '' };
    case 'comparator':
      // Depends on comparator type
      return '';
  }
  return {};
};

export const updateKey = ({
  permissionsState,
  newKey,
  keyPath,
  type,
}: {
  permissionsState: Pick<RowPermissionsState, 'permissions' | 'operators'>;
  newKey: string; // New key to be set
  keyPath: string[]; // Path to the key to be deleted
  type?: PermissionType;
}) => {
  // Clone deep so we don't run into issues with immutability
  // The issue happens when cloning with spread operator, after submitting and getting an error
  // Using cloneDeep we don't run into this issue
  const clone = cloneDeep(permissionsState);
  const path = getKeyPath({ permissionsState: clone, keyPath, newKey, type });
  const value = getInitialValue(newKey, type);

  const parentKey = path[path.length - 1];
  const parentIsArray = parseInt(parentKey);
  if (parentIsArray) {
    const prevPath = path.slice(0, -1);
    const obj = get(clone, prevPath);
    const filtered = obj.filter((o: Record<string, string>) => !isEmpty(o));
    return set(clone, prevPath, filtered.length ? filtered : [{}]);
  }

  return set(clone, path, value);
};

export const isComparator = (k: string) => {
  return allOperators.find(o => o.name === k);
};

export const isPrimitive = (value: any) => {
  return !isObjectLike(value);
};

export function isColumnComparator(comparator: string) {
  return (
    comparator === '_ceq' ||
    comparator === '_cne' ||
    comparator === '_cgt' ||
    comparator === '_cge' ||
    comparator === '_clt' ||
    comparator === '_cle'
  );
}

function isEmptyArray(value: string, previousKey: string) {
  return (
    Array.isArray(value) &&
    isEmpty(value) &&
    (previousKey === '_and' || previousKey === '_or' || previousKey === '_not')
  );
}
