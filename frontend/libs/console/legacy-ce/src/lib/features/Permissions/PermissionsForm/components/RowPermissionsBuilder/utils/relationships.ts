import { isNotRemoteRelationship } from '../../../../../DatabaseRelationships/utils/helpers';
import { Table } from '../../../../../hasura-metadata-types';
import { Relationships } from '../components';

/**
 * Get the relationship tables that are not already in the tablesToLoad array
 **/
export function getNewTablesToLoad({
  relationships,
  tablesToLoad,
}: {
  relationships: Relationships;
  tablesToLoad: Table[];
}): Table[] {
  const relatedTables = relationships.filter(isNotRemoteRelationship);

  // Create a Set of existing tables for faster lookups
  const existingTablesSet = new Set<string>(
    tablesToLoad.map(t => JSON.stringify(t))
  );

  // Use a Set to collect unique tables to add
  const tablesToAddSet = new Set<string>();

  relatedTables.forEach(relationship => {
    const toTableString = JSON.stringify(relationship.definition.toTable);

    // Check if the toTable is not in the existing tables
    if (!existingTablesSet.has(toTableString)) {
      tablesToAddSet.add(toTableString);
    }
  });

  // Convert the Set back to an array
  const tablesToAdd = Array.from(tablesToAddSet).map(table =>
    JSON.parse(table)
  );

  return tablesToAdd;
}
