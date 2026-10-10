import React, { useState } from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { Flex, Strong } from '@radix-ui/themes';
import {
  Button,
  Checkbox,
  CheckboxesField,
  Collapsible,
  CollapsibleHeader,
  IndicatorCard,
  Text,
} from '@hasura/shared/ui';
import { TableColumn, useTableColumns } from '@hasura/metadata/data-source';
import { PermissionsConfirmationModal } from './RootFieldPermissions/PermissionsConfirmationModal';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { isPermissionModalDisabled } from '../utils/getPermissionModalStatus';
import {
  getPermissionsModalTitle,
  getPermissionsModalDescription,
} from './RootFieldPermissions/PermissionsConfirmationModal.utils';
import {
  DataQueryType,
  MetadataTable,
  QualifiedDataSource,
  QueryRootPermissionType,
  SubscriptionRootPermissionType,
} from '@hasura/shared/types';
import { getEdForm } from '@hasura/shared/utils';

const getAccessText = (queryType: string) => {
  if (queryType === 'insert') {
    return 'to set input for';
  }

  if (queryType === 'select') {
    return 'to access';
  }

  return 'to update';
};

export interface ColumnPermissionsSectionProps {
  queryType: DataQueryType;
  roleName: string;
  columns?: string[];
  computedFields?: string[];
  table: MetadataTable;
  source: QualifiedDataSource;
}

const useStatus = (disabled: boolean) => {
  const { control } = useFormContext();
  const formColumns = useWatch({ control, name: 'columns' });

  if (!formColumns) {
    return { data: '', isError: false };
  }

  const columnValues = Object.values(formColumns);
  const selectedColumns = columnValues.filter((value) => value);

  if (disabled) {
    return { data: 'Disabled: Set row permissions first', isError: false };
  }

  if (selectedColumns?.length === 0) {
    return { data: 'No columns', isError: false };
  }

  if (selectedColumns?.length === columnValues?.length) {
    return { data: 'All columns', isError: false };
  }

  return { data: 'Partial columns', isError: false };
};

const checkIfConfirmationIsNeeded = (
  fieldName: string,
  tableColumns: TableColumn[],
  selectedColumns: Record<string, boolean>,
  queryRootFields: QueryRootPermissionType[],
  subscriptionRootFields: SubscriptionRootPermissionType[],
) => {
  const primaryKeys = tableColumns
    ?.filter((column) => column.isPrimaryKey)
    ?.map((column) => column.name);
  const pkRootFieldsAreSelected =
    queryRootFields?.includes('select_by_pk') ||
    subscriptionRootFields?.includes('select_by_pk');
  return (
    selectedColumns[fieldName] &&
    pkRootFieldsAreSelected &&
    primaryKeys.includes(fieldName)
  );
};

export const ColumnPermissionsSection: React.FC<
  ColumnPermissionsSectionProps
> = ({ roleName, queryType, columns, table, computedFields, source }) => {
  const { setValue, watch } = useFormContext();
  const [showConfirmation, setShowConfirmationModal] = useState<string | null>(
    null,
  );

  const [selectedColumns, queryRootFields, subscriptionRootFields] = watch([
    'columns',
    'query_root_fields',
    'subscription_root_fields',
  ]);

  // if no row permissions are selected selection should be disabled
  const disabled = useIsDisabled(queryType);

  const { data: status, isError } = useStatus(disabled);

  const { data: tableColumns } = useTableColumns({
    source,
    table: table.table,
  });

  const tableComputedFields = table.computed_fields?.map(({ name }) => name);

  const onClick = () => {
    columns?.forEach((column) => {
      const toggleAllOn = status !== 'All columns';
      // if status is not all columns: toggle all on
      // otherwise toggle all off
      setValue(`columns.${column}`, toggleAllOn);
    });
    computedFields?.forEach((field) => {
      const toggleAllOn = status !== 'All columns';
      // if status is not all columns: toggle all on
      // otherwise toggle all off
      setValue(`computed_fields.${field}`, toggleAllOn);
    });
  };

  if (isError) {
    return (
      <IndicatorCard status="negative" showIcon>
        Error loading column permission data
      </IndicatorCard>
    );
  }

  const handleUpdate = (fieldName: string) => {
    setValue(
      'query_root_fields',
      queryRootFields.filter((field: string) => field !== 'select_by_pk'),
    );
    setValue(
      'subscription_root_fields',
      subscriptionRootFields.filter(
        (field: string) => field !== 'select_by_pk',
      ),
    );
    setValue(`columns.${fieldName}`, !selectedColumns[fieldName]);
  };

  const permissionsModalTitle = getPermissionsModalTitle({
    scenario: 'pks',
    role: roleName,
    primaryKeyColumns: tableColumns?.columns
      ?.filter((column) => column.isPrimaryKey)
      ?.map((column) => column.name)
      ?.join(','),
  });

  const permissionsModalDescription = getPermissionsModalDescription('pks');

  return (
    <>
      <Collapsible
        defaultOpen={!disabled}
        triggerChildren={
          <CollapsibleHeader
            title={`Column ${queryType} permissions`}
            tooltip={`Choose columns allowed to be ${getEdForm(queryType)}`}
            status={status}
            // disabledMessage="Set row permissions first"
          />
        }
      >
        <div title={disabled ? 'Set row permissions first' : ''}>
          <div className="mb-2">
            <Text>
              Allow role <Strong>{roleName}</Strong> {getAccessText(queryType)}
              &nbsp;
              <Strong>columns</Strong>:
            </Text>
          </div>
          <Flex gap="4" wrap="wrap">
            {columns?.map((fieldName) => (
              <Checkbox
                key={fieldName}
                title={disabled ? 'Set a row permission first' : ''}
                disabled={disabled}
                value={selectedColumns[fieldName]}
                onChange={() => {
                  const hideModal = isPermissionModalDisabled();
                  if (
                    !hideModal &&
                    !showConfirmation &&
                    checkIfConfirmationIsNeeded(
                      fieldName,
                      tableColumns?.columns ?? [],
                      selectedColumns,
                      queryRootFields,
                      subscriptionRootFields,
                    )
                  ) {
                    setShowConfirmationModal(fieldName);
                    return;
                  }
                  setValue(`columns.${fieldName}`, !selectedColumns[fieldName]);
                }}
              >
                <Text className="italic">{fieldName}</Text>
              </Checkbox>
            ))}
            {queryType === 'select' && !!tableComputedFields?.length && (
              <CheckboxesField
                disabled={disabled}
                name="computed_fields"
                orientation="horizontal"
                noErrorPlaceholder
                options={tableComputedFields.map((fieldName) => ({
                  label: <Text className="italic">{fieldName}</Text>,
                  value: fieldName,
                  title: disabled ? 'Set a row permission first' : undefined,
                }))}
              />
            )}
            <Button
              type="button"
              size="sm"
              mode="default"
              title={disabled ? 'Set a row permission first' : ''}
              disabled={disabled}
              onClick={onClick}
              data-test="toggle-all-col-btn"
            >
              Toggle All
            </Button>
          </Flex>
        </div>
        {/* {getExternalTablePermissionsMsg()} */}
      </Collapsible>
      {showConfirmation && (
        <PermissionsConfirmationModal
          title={permissionsModalTitle}
          description={permissionsModalDescription}
          onClose={() => setShowConfirmationModal(null)}
          onSubmit={() => {
            handleUpdate(showConfirmation);
            setShowConfirmationModal(null);
          }}
        />
      )}
    </>
  );
};

export default ColumnPermissionsSection;
