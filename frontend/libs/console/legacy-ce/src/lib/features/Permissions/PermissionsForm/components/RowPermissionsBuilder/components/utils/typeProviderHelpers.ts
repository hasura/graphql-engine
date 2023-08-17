import { areTablesEqual } from '../../../../../../hasura-metadata-api';
import { Table } from '../../../../../../hasura-metadata-types';
import set from 'lodash/set';
import isObjectLike from 'lodash/isObjectLike';
import { PermissionType, Permissions, Tables } from '../types';
import { getNamedType, getNullableType, isObjectType } from 'graphql';

export function getPermissionTypes(
  tables: Tables,
  table: Table,
  permissions: Permissions
) {
  const newTypes: Record<string, { type: PermissionType }> = {};
  const setPermissionTypes = (value: any, path: string[]) => {
    const relationshipTable = tables.find(t => areTablesEqual(t.table, table));
    const currentTable = tables.find(t => areTablesEqual(t.table, table));
    const type = currentTable?.columns.find(
      c => c.name === path[path.length - 1]
    )?.graphQLProperties?.graphQLType;
    const pureType = type ? getNamedType(getNullableType(type)) : undefined;
    if (
      relationshipTable?.relationships.find(
        r => r.name === path[path.length - 1]
      )
    ) {
      set(newTypes, path.join('.'), { type: 'relationship' });
    } else if (pureType && isObjectType(pureType)) {
      // If it's an object but not on relationships it means it's a nested object
      set(newTypes, path.join('.'), { type: 'object' });
    }
    if (isObjectLike(value)) {
      if (Array.isArray(value)) {
        value.forEach((item, index) => {
          setPermissionTypes(item, [...path, index.toString()]);
        });
      } else {
        Object.keys(value).forEach(key => {
          setPermissionTypes(value[key], [...path, key]);
        });
      }
    }
  };
  setPermissionTypes(permissions, []);
  return newTypes;
}
