import { isNotRemoteSchemaRelationship } from '../../../../../DatabaseRelationships/utils/helpers';
import { Relationships, TableToLoad } from '../components';

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
  const relatedTables = relationships.filter(isNotRemoteSchemaRelationship);

  // Create a Set of existing tables for faster lookups
  const existingTablesSet = new Set<string>(
    tablesToLoad.map(t => JSON.stringify(t))
  );

  // Use a Set to collect unique tables to add
  const tablesToAddSet = new Set<string>();

  relatedTables.forEach(relationship => {
    let tableObject = undefined;
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
    if (!existingTablesSet.has(tableObject)) {
      tablesToAddSet.add(tableObject);
    }
  });

  // Convert the Set back to an array
  const tablesToAdd = Array.from(tablesToAddSet).map(table =>
    JSON.parse(table)
  );

  return tablesToAdd;
}
