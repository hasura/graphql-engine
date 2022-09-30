import {
  allowedMetadataTypes,
  MetadataTableConfig,
} from '@/features/MetadataAPI';
import { TrackableTable } from '../types';

export const buildTrackingQuery = (
  dataSourceName: string,
  resourceVersion: number,
  driver: string,
  action: 'track' | 'untrack',
  tables: TrackableTable[]
): TrackingQuery => ({
  type: 'bulk',
  source: dataSourceName,
  resource_version: resourceVersion,
  args: tables.map((table: TrackableTable) => ({
    type: `${driver}_${action}_table`,
    args: {
      source: dataSourceName,
      table: table.table,
      ...(action === 'track' &&
        table.configuration && { configuration: table.configuration }),
    },
  })),
});

export type TrackingQuery = {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: QueryArgs[];
};

type QueryArgs = {
  type: allowedMetadataTypes;
  args: {
    source: string;
    table: unknown;
    configuration?: MetadataTableConfig;
  };
};
