import { MetadataSelectors, useMetadata } from '@/features/hasura-metadata-api';
import { Tables } from '../components';
import { MetadataTable } from '@/features/hasura-metadata-types';
import { columnsFromSchema } from '../components/utils/columnsFromSchema';
import { getTableDisplayName } from '@/features/DatabaseRelationships';
import { useIntrospectSchema } from '.';

export const usePermissionTables = ({
  dataSourceName,
  tableCustomName,
}: {
  dataSourceName: string;
  tableCustomName: string | undefined;
}) => {
  const { data: tables } = useMetadata(
    MetadataSelectors.getTables(dataSourceName)
  );
  const { data: schema } = useIntrospectSchema();
  if (!tables) return [];
  const allColumns = columnsFromSchema(schema);
  return tables.map(table => {
    // Table name. Replace . with _ because GraphQL doesn't allow . in field names
    const tableName = getTableDisplayName(table.table).replace(/\./g, '_');
    const customName = tableCustomName ?? tableName;

    return {
      table: table.table,
      dataSource: dataSourceName,
      relationships: tableRelationships(table),
      columns: allColumns[customName] ?? [],
    };
  });
};

type Relationships = Tables[number]['relationships'];

function tableRelationships(table: MetadataTable): Relationships {
  const relationships = [] as Relationships;
  if (table.array_relationships) {
    relationships.push(
      ...table.array_relationships.map(r => {
        const relatedTable =
          'manual_configuration' in r.using
            ? r.using.manual_configuration.remote_table
            : r.using.foreign_key_constraint_on.table;
        return {
          name: r.name,
          type: 'array' as const,
          table: relatedTable,
        };
      })
    );
  }
  if (table.object_relationships) {
    relationships.push(
      ...table.object_relationships.map(r => {
        const relatedTable =
          'manual_configuration' in r.using
            ? r.using.manual_configuration.remote_table
            : Array.isArray(r.using.foreign_key_constraint_on) ||
              typeof r.using.foreign_key_constraint_on === 'string'
            ? r.using.foreign_key_constraint_on
            : r.using.foreign_key_constraint_on.table;
        return {
          name: r.name,
          type: 'object' as const,
          table: relatedTable,
        };
      })
    );
  }
  return relationships;
}
