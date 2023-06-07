import isEqual from 'lodash/isEqual';

import { TableColumn } from '../../../../../../DataSource';
import { getTypeName } from '../../../../../../GraphQLUtils';

import { Metadata } from '../../../../../../hasura-metadata-types';

import { PermissionsSchema } from '../../../../../schema';

import type { QueryType } from '../../../../../types';
import { SourceCustomization } from '../../../../../../hasura-metadata-types/source/source';
import { Operator } from '../../../../../../DataSource/types';

import {
  MetadataDataSource,
  TableEntry,
} from '../../../../../../../metadata/types';

import { createPermissionsObject } from './utils';

interface GetMetadataTableArgs {
  table: unknown;
  trackedTables: TableEntry[] | undefined;
}

const getMetadataTable = ({ table, trackedTables }: GetMetadataTableArgs) => {
  // find selected table
  const currentTable = trackedTables?.find(trackedTable =>
    isEqual(trackedTable.table, table)
  );

  return currentTable;
};

export interface CreateDefaultValuesArgs {
  queryType: QueryType;
  roleName: string;
  table: unknown;
  dataSourceName: string;
  metadata: Metadata;
  tableColumns: TableColumn[];
  defaultQueryRoot: string | never[];
  metadataSource: MetadataDataSource | undefined;
  supportedOperators: Operator[];
}

export const createDefaultValues = ({
  queryType,
  roleName,
  table,
  tableColumns,
  defaultQueryRoot,
  metadataSource,
  supportedOperators,
}: CreateDefaultValuesArgs) => {
  const selectedTable = getMetadataTable({
    table,
    trackedTables: metadataSource?.tables,
  });

  const tableName = getTypeName({
    defaultQueryRoot,
    operation: 'select',
    sourceCustomization: metadataSource?.customization as SourceCustomization,
    configuration: selectedTable?.configuration,
  });

  const baseDefaultValues: DefaultValues = {
    queryType: 'select',
    filterType: 'none',
    columns: {},
    supportedOperators,
  };

  if (selectedTable) {
    const permissionsObject = createPermissionsObject({
      queryType,
      selectedTable,
      roleName,
      tableColumns,
      tableName,
      metadataSource,
    });

    return { ...baseDefaultValues, ...permissionsObject };
  }

  return baseDefaultValues;
};

type DefaultValues = PermissionsSchema & {
  operators?: Record<string, unknown>;
};
