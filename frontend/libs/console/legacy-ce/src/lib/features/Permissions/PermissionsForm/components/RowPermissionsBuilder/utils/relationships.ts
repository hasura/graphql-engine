import { isNotRemoteSchemaRelationship } from '../../../../../DatabaseRelationships/utils/helpers';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { Table } from '@hasura/shared/types';
import { Permissions, Relationships, TableToLoad } from '../components';

/**
 * Get the relationship tables that are not already in the tablesToLoad array
 **/
export function getNewTablesToLoad({
  relationships,
  tablesToLoad,
}: {
  relationships: Relationships;
  tablesToLoad: TableToLoad;
}): TableToLoad {
  // a relationship on a foreign key has no target until its table is loaded
  const relatedTables = relationships
    .filter(isNotRemoteSchemaRelationship)
    .filter((relationship) => !!relationship.definition.toTable);

  // Create a Set of existing tables for faster lookups
  const existingTablesSet = new Set<string>(
    tablesToLoad.map((t) => JSON.stringify(t)),
  );

  // Use a Set to collect unique tables to add
  const tablesToAddSet = new Set<string>();

  relatedTables.forEach((relationship) => {
    let tableObject: string;
    if (
      'toSource' in relationship.definition &&
      typeof relationship.definition.toTable === 'object'
    ) {
      tableObject = JSON.stringify({
        source: relationship.definition.toSource,
        table: relationship.definition.toTable,
      });
    } else {
      tableObject = JSON.stringify({
        table: relationship.definition.toTable,
        source: relationship.fromSource,
      });
    }
    // Check if the toTable is not in the existing tables
    if (tableObject && !existingTablesSet.has(tableObject)) {
      tablesToAddSet.add(tableObject);
    }
  });

  // Convert the Set back to an array
  const tablesToAdd = Array.from(tablesToAddSet).map((table) =>
    JSON.parse(table),
  );

  return tablesToAdd;
}

/**
 * Recursively collect every table referenced by an `_exists` operator's
 * `_table` inside a permission filter (including `_exists` nested under
 * `_and`/`_or`/`_not` or inside another `_exists`'s `_where`).
 *
 * Unlike relationships, an `_exists` target table is not discovered while
 * loading a table's relationships, so without collecting them here their
 * columns are never introspected and the `_where` conditions render blank.
 */
export function collectExistsTables(permissions: Permissions): Table[] {
  const found: Table[] = [];

  const visit = (node: unknown) => {
    if (!node || typeof node !== 'object') {
      return;
    }

    if (Array.isArray(node)) {
      node.forEach(visit);
      return;
    }

    const record = node as Record<string, unknown>;
    const exists = record._exists;
    if (exists && typeof exists === 'object' && !Array.isArray(exists)) {
      const existsTable = (exists as Record<string, unknown>)._table;
      if (
        existsTable &&
        typeof existsTable === 'object' &&
        Object.keys(existsTable).length > 0
      ) {
        found.push(existsTable as Table);
      }
    }

    Object.values(record).forEach(visit);
  };

  visit(permissions);
  return found;
}

/**
 * The `_exists` target tables of a filter that are not already queued to load.
 */
export function getNewExistsTablesToLoad({
  permissions,
  tablesToLoad,
  source,
}: {
  permissions: Permissions;
  tablesToLoad: TableToLoad;
  source: string;
}): TableToLoad {
  const tablesToAdd: TableToLoad = [];

  collectExistsTables(permissions).forEach((table) => {
    const alreadyQueued =
      tablesToLoad.some(
        (t) => t.source === source && areTablesEqual(t.table, table),
      ) || tablesToAdd.some((t) => areTablesEqual(t.table, table));

    if (!alreadyQueued) {
      tablesToAdd.push({ table, source });
    }
  });

  return tablesToAdd;
}
