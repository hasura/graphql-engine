import { areTablesEqual } from '../../../../../../hasura-metadata-api';
import { Table } from '../../../../../../hasura-metadata-types';
import set from 'lodash/set';
import isObjectLike from 'lodash/isObjectLike';
import { Permissions, Tables } from '../types';

export const getPermissionTypes = (
  tables: Tables,
  table: Table,
  permissions: Permissions
) => {
  const newTypes = {};
  const setPermissionTypes = (value: any, path: string[]) => {
    const relationshipTable = tables.find(t => areTablesEqual(t.table, table));
    if (
      relationshipTable?.relationships.find(
        r => r.name === path[path.length - 1]
      )
    ) {
      set(newTypes, path.join('.'), { type: 'relationship' });
    }
    if (isObjectLike(value)) {
      if (Array.isArray(value)) {
        setPermissionTypes(value[0], [...path, '0']);
      } else {
        Object.keys(value).forEach(key => {
          setPermissionTypes(value[key], [...path, key]);
        });
      }
    }
  };
  setPermissionTypes(permissions, []);
  return newTypes;
};
