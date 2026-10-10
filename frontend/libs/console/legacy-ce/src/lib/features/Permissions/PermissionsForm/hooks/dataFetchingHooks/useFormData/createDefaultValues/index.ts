import isEqual from 'lodash/isEqual';
import { TableColumn, Operator } from '@hasura/metadata/data-source';
import { getTypeName } from '@hasura/shared/utils';
import { PermissionsSchema } from '../../../../../schema';
import {
  SourceCustomization,
  DataQueryType,
  ComputedField,
  Source,
  MetadataTable,
} from '@hasura/shared/types';
import { createPermissionsObject, DefaultPermissionValues } from './utils';
import { TablePermissionInputValidationSchema } from '../../../../components/InputValidation/InputValidation';

interface GetMetadataTableArgs {
  table: unknown;
  trackedTables: MetadataTable[] | undefined;
}

const getMetadataTable = ({ table, trackedTables }: GetMetadataTableArgs) => {
  // find selected table
  const currentTable = trackedTables?.find((trackedTable) =>
    isEqual(trackedTable.table, table),
  );

  return currentTable;
};

export interface CreateDefaultValuesArgs {
  queryType: DataQueryType;
  roleName: string;
  table: unknown;
  dataSourceName: string;
  tableColumns: TableColumn[];
  tableComputedFields: ComputedField[];
  defaultQueryRoot: string | never[];
  metadataSource: Source | undefined;
  supportedOperators: Operator[];
  validateInput: TablePermissionInputValidationSchema;
}

export const createDefaultValues = ({
  queryType,
  roleName,
  table,
  tableColumns,
  tableComputedFields,
  defaultQueryRoot,
  metadataSource,
  supportedOperators,
  validateInput,
}: CreateDefaultValuesArgs): PermissionsSchema => {
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

  const baseDefaultValues: DefaultPermissionValues = {
    queryType: 'select',
    comment: '',
    filterType: 'none',
    filter: undefined,
    columns: {},
    computed_fields: [],
    supportedOperators,
    validateInput,
    aggregationEnabled: false,
    customRootFieldEnabled: false,
    query_root_fields: [],
    subscription_root_fields: [],
  };

  if (selectedTable) {
    const permissionsObject = createPermissionsObject({
      queryType,
      selectedTable,
      roleName,
      tableColumns,
      tableComputedFields,
      tableName,
      metadataSource,
    });

    return Object.assign(baseDefaultValues, permissionsObject);
  }

  return baseDefaultValues;
};
