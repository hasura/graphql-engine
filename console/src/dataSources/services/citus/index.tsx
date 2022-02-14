import { DeepRequired } from 'ts-essentials';

import { DataSourcesAPI } from '../..';
import {
  getFetchTablesListQuery,
  schemaListSql,
  schemaListQuery,
} from './sqlUtils';
import {
  generateTableRowRequest,
  generateInsertRequest,
  generateRowsCountRequest,
  generateEditRowRequest,
  generateDeleteRowRequest,
  generateBulkDeleteRowRequest,
} from './utils';
import { SupportedFeaturesType } from '../../types';
import {
  postgres,
  supportedFeatures as PgSupportedFeatures,
} from '../postgresql';

export const supportedFeatures: DeepRequired<SupportedFeaturesType> = {
  ...PgSupportedFeatures,
  driver: {
    name: 'citus',
    fetchVersion: {
      enabled: false,
    },
  },
  tables: {
    ...(PgSupportedFeatures?.tables ?? {}),
    browse: {
      enabled: true,
      aggregation: true,
      customPagination: true,
    },
    modify: {
      ...(PgSupportedFeatures?.tables?.modify ?? {}),
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
      enabled: true,
      add: false,
    },
  },
  actions: {
    enabled: true,
    relationships: false,
  },
  functions: {
    enabled: true,
    track: {
      enabled: true,
    },
    nonTrackableFunctions: {
      enabled: true,
    },
    modify: {
      enabled: true,
      comments: {
        view: true,
        edit: true,
      },
    },
  },
};

export const citus: DataSourcesAPI = {
  ...postgres,
  supportedFeatures,
  schemaListSql,
  getFetchTablesListQuery,
  generateTableRowRequest,
  generateInsertRequest,
  generateRowsCountRequest,
  generateEditRowRequest,
  generateDeleteRowRequest,
  generateBulkDeleteRowRequest,
  schemaListQuery,
};
