import { Table } from '@hasura/shared/types';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { Relationships, Tables } from '../types';

/**
 * Finds a table, preferring the one of the given source: sources can track
 * tables with the same name.
 */
export const findTable = (
  tables: Tables,
  table: Table | undefined,
  source?: string,
) => {
  if (!table) return undefined;
  const matches = tables.filter((t) => areTablesEqual(t.table, table));
  return matches.find((t) => t.dataSource?.name === source) ?? matches[0];
};

/** The source of the table a relationship points to. */
export const getRelationshipTargetSource = (
  relationship: Relationships[number],
) =>
  relationship.type === 'remoteDatabaseRelationship'
    ? relationship.definition.toSource
    : relationship.fromSource;
