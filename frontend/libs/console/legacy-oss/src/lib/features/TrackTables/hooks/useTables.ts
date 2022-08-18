import { DataSource, Feature, exportMetadata } from '@/features/DataSource';
import type { IntrospectedTable, MetadataTable } from '@/features/DataSource';
import type { MetadataDataSource } from '@/metadata/types';
import uniqueId from 'lodash.uniqueid';
import { useQuery } from 'react-query';
import { useHttpClient } from '@/features/Network';
import { useAppSelector } from '@/store';

type DataSource = Pick<MetadataDataSource, 'name'>;

export type UseTablesProps = {
  dataSource: DataSource;
};

export type TrackableTable = {
  /**
   *	Useful for ID'ing unique table rows for checkboxes and solo-updates for
   *	individual rows.
   */
  id: string;

  /**
   *	Self explanatory. Used for the display name.
   */
  name: string;

  /**
   *	A `table` represent the generic json that is used by metadata to reference
   *	a tracked table. Its best to not mess with this object and just treat it like
   *	a black box, passing it back to the server while tracking/untracking
   */
  table: Record<string, string>;

  /**
   *	Right now, we don't have a strong use case for the UI, but it's handy to have this info
   *	while tracking tables, Like put it on badge under the table. Maybe could be even
   *	used for filtering in the future versions.
   */
  type: string;
} & (
  | {
      /* Represent whether a table is tracked or not, pretty self explanatory */
      is_tracked: false;
    }
  | {
      is_tracked: true;

      /**
       * Configuration data for a tracked table. Should be applicable only after the
       * table is tracked.
       */
      configuration?: {
        custom_root_fields?: {
          select?: string;
          select_by_pk?: string;
          select_aggregate?: string;
          insert?: string;
          insert_one?: string;
          update?: string;
          update_by_pk?: string;
          delete?: string;
          delete_by_pk?: string;
        };
        column_config?: Record<
          string,
          { custom_name: string; comment: string }
        >;
        comment?: string;
      };
    }
);

const getTrackableTables = (
  trackedTables: MetadataTable[],
  introspectedTables: IntrospectedTable[]
) =>
  introspectedTables.map(introspectedTable => {
    const trackedTable = trackedTables.find(
      _trackedTable =>
        `${_trackedTable.table.schema}.${_trackedTable.table.name}` ===
        introspectedTable.name
    );

    const isTracked = !!trackedTable;
    if (isTracked) {
      const trackableTable: TrackableTable = {
        id: uniqueId(),
        name: introspectedTable.name,
        table: introspectedTable.table,
        type: introspectedTable.type,
        is_tracked: isTracked,
        configuration: {
          custom_root_fields: trackedTable?.configuration?.custom_root_fields,
        },
      };
      return trackableTable;
    }

    const trackableTable: TrackableTable = {
      id: uniqueId(),
      name: introspectedTable.name,
      table: introspectedTable.table,
      type: introspectedTable.type,
      is_tracked: isTracked,
    };
    return trackableTable;
  });

export const useTables = ({ dataSource }: UseTablesProps) => {
  const headers = useAppSelector(state => state.tables.dataHeaders);
  const httpClient = useHttpClient({ headers });
  return useQuery<TrackableTable[], Error>({
    queryKey: [dataSource.name, 'tables'],
    queryFn: async () => {
      const metadata = await exportMetadata({
        httpClient,
      });
      const currentMetadataSource = metadata.sources?.find(
        source => source.name === dataSource.name
      );

      if (!currentMetadataSource)
        throw Error(`useTables.metadataSource not found`);

      const introspectedTables = await DataSource(httpClient).introspectTables({
        dataSourceName: dataSource.name,
      });

      if (introspectedTables === Feature.NotImplemented)
        throw Error(
          `useTables.introspectedTables Feature is not available for ${currentMetadataSource.kind}`
        );

      const trackedTables = currentMetadataSource.tables;

      const trackableTables = getTrackableTables(
        trackedTables,
        introspectedTables
      );

      return trackableTables;
    },
  });
};
