import { IndicatorCard } from '@hasura/shared/ui';
import { createDefaultValues } from '../../hooks';
import { createFormData } from '../../hooks/dataFetchingHooks/useFormData/createFormData/index';
import {
  AccessType,
  DataQueryType,
  Metadata,
  MetadataTable,
  Source,
} from '@hasura/shared/types';
import PermissionsFormComponent from './PermissionsFormComponent';
import { Skeleton } from '@radix-ui/themes';
import {
  getDatabaseMethods,
  useTableColumns,
} from '@hasura/metadata/data-source';
import { getErrorMessage, parseHeaderConfigs } from '@hasura/shared/utils';

interface PermissionsFormWrapperProps {
  metadata: Metadata['metadata'];
  source: Source;
  table: MetadataTable;
  queryType: DataQueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
  showCloseButton?: boolean;
}

// necessary to wrap in this component as otherwise default values are not set properly in useConsoleForm
const PermissionsFormWrapper = ({
  metadata,
  source: dataSource,
  table: metadataTable,
  queryType,
  roleName,
  accessType,
  handleClose,
  showCloseButton,
}: PermissionsFormWrapperProps) => {
  const {
    data: tableColumns,
    isLoading: isLoadingTables,
    error,
  } = useTableColumns({
    source: dataSource,
    table: metadataTable.table,
  });

  if (isLoadingTables || !tableColumns) {
    return <Skeleton width="100%" height="300px" />;
  }

  if (error) {
    return (
      <IndicatorCard status="negative" headline="Error">
        {error ? getErrorMessage(error) : 'Source not found'}
      </IndicatorCard>
    );
  }

  const computedFields = metadataTable?.computed_fields ?? [];

  const databaseMethods = getDatabaseMethods(dataSource.kind);
  const defaultQueryRoot = databaseMethods.config.getDefaultQueryRoot({
    table: metadataTable.table,
  });

  const supportedOperators =
    databaseMethods.introspection.getSupportedOperators();

  const validateInput = getValidateInput(metadataTable, queryType, roleName);

  const defaultValues = createDefaultValues({
    queryType,
    roleName,
    dataSourceName: dataSource.name,
    table: metadataTable.table,
    tableColumns: tableColumns.columns,
    tableComputedFields: computedFields,
    defaultQueryRoot: defaultQueryRoot,
    metadataSource: dataSource,
    supportedOperators: supportedOperators,
    validateInput: validateInput
      ? {
          enabled: true,
          type: 'http',
          definition: {
            url: validateInput.definition?.url,
            forward_client_headers:
              validateInput.definition.forward_client_headers,
            headers: parseHeaderConfigs(validateInput.definition.headers),
            timeout: validateInput.definition.timeout ?? undefined,
          },
        }
      : { enabled: false },
  });

  const formData = createFormData({
    table: metadataTable.table,
    tableColumns: tableColumns.columns,
    computedFields,
    source: dataSource,
    validateInput: {
      enabled: false,
    },
  });

  return (
    <PermissionsFormComponent
      metadata={metadata}
      defaultValues={defaultValues}
      formData={formData}
      dataSource={dataSource}
      accessType={accessType}
      handleClose={handleClose}
      table={metadataTable}
      queryType={queryType}
      roleName={roleName}
      showCloseButton={showCloseButton}
    />
  );
};

const getValidateInput = (
  permissions: MetadataTable | undefined,
  queryType: string,
  roleName: string,
) => {
  switch (queryType) {
    case 'insert':
      return permissions?.insert_permissions?.find(
        (permission) => permission.role === roleName,
      )?.permission?.validate_input;
    case 'update':
      return permissions?.update_permissions?.find(
        (permission) => permission.role === roleName,
      )?.permission?.validate_input;
    case 'delete':
      return permissions?.delete_permissions?.find(
        (permission) => permission.role === roleName,
      )?.permission?.validate_input;
  }
};

export default PermissionsFormWrapper;
