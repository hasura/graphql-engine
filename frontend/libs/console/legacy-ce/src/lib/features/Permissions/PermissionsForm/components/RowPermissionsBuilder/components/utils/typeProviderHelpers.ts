import { areTablesEqual } from '../../../../../../hasura-metadata-api';
import { Table } from '../../../../../../hasura-metadata-types';
import set from 'lodash/set';
import isObjectLike from 'lodash/isObjectLike';
import { PermissionType, Permissions, Tables } from '../types';
import { getNamedType, getNullableType, isObjectType } from 'graphql';

export function getPermissionTypes(
  tables: Tables,
  rootTable: Table,
  permissions: Permissions
) {
  const newTypes: Record<string, { type: PermissionType }> = {};
  const setPermissionTypes = (
    value: any,
    path: string[],
    table: Table,
    isRelationship: boolean
  ) => {
    const currentTable = tables.find(t => areTablesEqual(t.table, table));
    const type = currentTable?.columns.find(
      c => c.name === path[path.length - 1]
    )?.graphQLProperties?.graphQLType;
    const pureType = type ? getNamedType(getNullableType(type)) : undefined;
    if (isRelationship) {
      set(newTypes, path.join('.'), { type: 'relationship' });
    } else if (pureType && isObjectType(pureType)) {
      // If it's an object but not on relationships it means it's a nested object
      set(newTypes, path.join('.'), { type: 'object' });
    }
    if (isObjectLike(value)) {
      if (Array.isArray(value)) {
        value.forEach((item, index) => {
          setPermissionTypes(item, [...path, index.toString()], table, false);
        });
      } else {
        Object.keys(value).forEach(key => {
          const rel = currentTable?.relationships.find(r => r.name === key);
          setPermissionTypes(
            value[key],
            [...path, key],
            rel && 'toTable' in rel.definition ? rel.definition.toTable : table,
            !!rel
          );
        });
      }
    }
  };
  setPermissionTypes(permissions, [], rootTable, false);
  return newTypes;
}
