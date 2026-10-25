import { Table } from '@hasura/shared/types';
import set from 'lodash/set';
import isObjectLike from 'lodash/isObjectLike';
import { PermissionType, Permissions, Tables } from '../types';
import { getNamedType, getNullableType, isObjectType } from 'graphql';
import { findTable, getRelationshipTargetSource } from './tables';

export function getPermissionTypes(
  tables: Tables,
  rootTable: Table | undefined,
  permissions: Permissions,
  rootSource?: string,
) {
  const newTypes: Record<string, { type: PermissionType }> = {};
  const setPermissionTypes = (
    value: any,
    path: string[],
    table: Table | undefined,
    source: string | undefined,
    isRelationship: boolean,
  ) => {
    const currentTable = table ? findTable(tables, table, source) : undefined;
    const type = currentTable?.columns.find(
      (c) => c.name === path[path.length - 1],
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
          setPermissionTypes(
            item,
            [...path, index.toString()],
            table,
            source,
            false,
          );
        });
      } else {
        Object.keys(value).forEach((key) => {
          const rel = currentTable?.relationships.find((r) => r.name === key);
          setPermissionTypes(
            value[key],
            [...path, key],
            rel && 'toTable' in rel.definition ? rel.definition.toTable : table,
            rel ? getRelationshipTargetSource(rel) : source,
            !!rel,
          );
        });
      }
    }
  };
  setPermissionTypes(permissions, [], rootTable, rootSource, false);
  return newTypes;
}
