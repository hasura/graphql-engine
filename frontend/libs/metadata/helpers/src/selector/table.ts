import { MetadataTable, QualifiedTable } from '@hasura/shared/types';
import { extractTableInfo } from '@hasura/shared/utils';

export function findMetadataTableCoarse(
  tables: MetadataTable[] | undefined,
  { schema, name }: Partial<QualifiedTable>,
) {
  if (!tables?.length || !schema || !name) {
    return undefined;
  }

  return tables.find((t) => {
    const info = extractTableInfo(t.table);
    return info && info.schema === schema && info.name === name;
  });
}
