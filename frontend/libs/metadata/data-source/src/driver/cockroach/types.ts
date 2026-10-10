import { DeepRequired } from '@hasura/shared/types';
import { SupportedFeaturesType } from '../types';
import { postgresSupportedFeatures } from '../common/capabilities';

export type CockroachDBTable = { name: string; schema: string };

export const cockroachSupportedFeatures: DeepRequired<SupportedFeaturesType> = {
  ...postgresSupportedFeatures,
  connectDbForm: {
    ...postgresSupportedFeatures.connectDbForm,
    connectionParameters: false,
    enabled: false,
    namingConvention: false,
    extensions_schema: false,
  },
  tables: {
    ...postgresSupportedFeatures.tables,
    browse: {
      enabled: true,
      aggregation: true,
      customPagination: true,
      deleteRow: true,
      editRow: true,
      bulkRowSelect: true,
    },
    modify: {
      ...postgresSupportedFeatures.tables.modify,
      enabled: true,
      computedFields: false,
      triggers: false,
      customGqlRoot: true,
      setAsEnum: true,
      untrack: true,
      delete: true,
      indexes: {
        edit: false,
        view: false,
      },
    },
  },
  events: {
    triggers: {
      add: false,
    },
  },
  actions: {
    relationships: false,
  },
  functions: {
    track: {
      enabled: false,
    },
  },
};
