import { TrackableTable } from '@hasura/metadata/api';
import { MetadataTable } from '@hasura/shared/types';
import { extractTableInfo } from '@hasura/shared/utils';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { IntrospectedTable } from '../../driver';

type Payload = {
  trackedTables: TrackableTable[];
  untrackedTables: TrackableTable[];
};

export const splitByTracked = ({
  metadataTables,
  introspectedTables,
  schemaFilter,
}: {
  metadataTables: MetadataTable[];
  introspectedTables: IntrospectedTable[];
  schemaFilter?: string;
}): Payload => {
  return introspectedTables.reduce<Payload>(
    (payload, table) => {
      const qualifiedTable = extractTableInfo(table.table);

      if (schemaFilter && qualifiedTable?.name !== schemaFilter) {
        // skip if we have a schema filter active and the qualified table array does not include the schema name
        return payload;
      }

      const trackedTable = metadataTables.find((t) =>
        areTablesEqual(t.table, table.table),
      );

      const key: keyof Payload = trackedTable
        ? 'trackedTables'
        : 'untrackedTables';

      payload[key] = [
        ...payload[key],
        {
          ...table,
          id: table.name,
          is_tracked: !!trackedTable,
        },
      ];

      return payload;
    },
    { trackedTables: [], untrackedTables: [] },
  );
};
