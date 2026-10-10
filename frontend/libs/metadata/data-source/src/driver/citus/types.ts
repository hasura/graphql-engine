import { postgresSupportedFeatures } from '../common/capabilities';

export type CitusTable = { name: string; schema: string };

export const citusSupportedFeatures = {
  ...postgresSupportedFeatures,
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
      computedFields: true,
      triggers: true,
      customGqlRoot: true,
      setAsEnum: false,
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
      enabled: true,
    },
    modify: {
      comments: {
        view: true,
        edit: true,
      },
    },
  },
  connectDbForm: {
    ...postgresSupportedFeatures.connectDbForm,
    namingConvention: false,
    extensions_schema: false,
  },
};
