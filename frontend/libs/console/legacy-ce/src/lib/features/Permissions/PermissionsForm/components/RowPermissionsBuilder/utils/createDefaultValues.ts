import { createOperatorsObject } from '../../../PermissionsForm.utils';
import { TableColumn } from '../../../../../DataSource';
import { MetadataDataSource } from '../../../../../../metadata/types';

export interface CreateOperatorsArgs {
  tableName: string;
  existingPermission?: Record<string, any>;
}
export interface CreateDefaultsArgs {
  tableName: string;
  existingPermission?: Record<string, any>;
  sourceMetadata: MetadataDataSource | undefined;
  tableColumns: TableColumn[];
}

export const createDefaultValues = (props: CreateDefaultsArgs) => {
  const { tableName, existingPermission, tableColumns, sourceMetadata } = props;
  if (!existingPermission) {
    return {};
  }

  const operators = createOperatorsObject({
    tableName,
    existingPermission,
    tableColumns,
    sourceMetadataTables: sourceMetadata?.tables,
  });

  return {
    operators: {
      filter: operators,
    },
    filter: existingPermission,
    comment: existingPermission.comment,
  };
};
