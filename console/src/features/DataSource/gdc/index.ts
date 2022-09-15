import { Database, Feature, Property } from '../index';
import {
  getTablesListAsTree,
  getTrackableTables,
  getTableColumns,
} from './introspection';

/**
 * Why is GDCTable as string[] ?
 * It denotes the table along with it's hierarachy based on the DB. For example, in a mysql source
 * you'd have just the table name -> ["Album"] but in a db with schemas -> ["Public", "Album"].
 */
export type GDCTable = string[];

export const gdc: Database = {
  introspection: {
    getDriverInfo: async () => Feature.NotImplemented,
    getDatabaseConfiguration: async () => {
      /**
       * Once we have the API for fetching capabilities via HGE, we can make the correct call
       */
      const res = await fetch('http://localhost:8100/capabilities', {
        headers: {
          'x-hasura-dataConnector-config': JSON.stringify('./chinook.db'),
        },
      });

      const data: {
        configSchemas: {
          configSchema: Property;
          otherSchemas: Record<string, any>;
        };
      } = await res.json();

      return data.configSchemas;
    },
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      /**
       * Once we have the API for fetching the hierarchy info via HGE, we can add that logic here
       */
      return Feature.NotImplemented;
    },
    getTableColumns,
    getFKRelationships: async () => Feature.NotImplemented,
    getTablesListAsTree,
  },
};
