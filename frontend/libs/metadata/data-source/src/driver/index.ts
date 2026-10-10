import { SupportedDriver } from '@hasura/shared/types';
import type { AlloyDbTable } from './alloydb';
import { DataSourceNetworkArgs } from './types';
import { getTableName } from './common/getTableName';
import { exportMetadata } from '@hasura/metadata/api';
import { getDatabaseKind, getDatabaseMethods } from './dataSources';

export * from './types/database';
export * from './utils';
export * from './common/utils';
export * from './common/sqlUtils';
export * from './common/validation';
export * from './guards';
export * from './postgres';
export * from './types';
export * from './dataSources';

export { getTableName, type AlloyDbTable };

export const DataSource = (
  args: DataSourceNetworkArgs & { driver: SupportedDriver },
) => {
  const database = getDatabaseMethods(getDatabaseKind(args.driver));

  return {
    introspectTables: async ({
      dataSourceName,
    }: {
      dataSourceName: string;
    }) => {
      const { metadata } = await exportMetadata({
        fetchJson: args.fetchJson,
        url: args.endpoints.metadata,
      });

      const dataSource = metadata.sources.find(
        (source) => source.name === dataSourceName,
      );

      if (!dataSource) {
        throw Error(`${dataSourceName} not found in metadata`);
      }

      /*
        NOTE: We need a set of metadata types. Until then dataSource is type-casted to `any` because `configuration` varies from DB to DB and the old metadata types contain
        only pg databases at the moment. Changing the old types will require us to modify multiple legacy files
      */

      const getTrackableTables = database.introspection?.getTrackableTables;

      if (getTrackableTables) {
        return getTrackableTables({
          dataSourceName: dataSource.name,
          configuration: dataSource.configuration,
          ...args,
        });
      }

      return [];
    },
    // getIsTableView: async ({
    //   dataSourceName,
    //   table,
    // }: {
    //   dataSourceName: string;
    //   table: Table;
    // }) => {
    //   const introspection = database.introspection;

    //   if (!introspection) return false;

    //   const isView = await introspection.getIsTableView({
    //     dataSourceName,
    //     table,
    //     ...args,
    //   });

    //   if (isView === Feature.NotImplemented) return false;

    //   return isView;
    // },
  };
};
