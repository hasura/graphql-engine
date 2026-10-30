import React, { useState } from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { Flex, Strong } from '@radix-ui/themes';
import {
  Button,
  Checkbox,
  CheckboxesField,
  Collapsible,
  CollapsibleHeader,
  IconTooltip,
  IndicatorCard,
  Text,
} from '@hasura/shared/ui';
import { TableColumn, useTableColumns } from '@hasura/metadata/data-source';
import { PermissionsConfirmationModal } from './RootFieldPermissions/PermissionsConfirmationModal';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { useTableReturningComputedFields } from '../hooks/useTableReturningComputedFields';
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

  // computed fields returning a table derive their select permissions from
  // that table, so the server rejects them in `computed_fields`
  const { data: tableReturningComputedFields = [] } =
    useTableReturningComputedFields({
      source,
      computedFields: table.computed_fields ?? [],
    });

  const onClick = () => {
    // if status is not all columns: toggle all on, otherwise toggle all off
    const toggleAllOn = status !== 'All columns';
    columns?.forEach((column) => {
      setValue(`columns.${column}`, toggleAllOn);
    });
    // `computed_fields` is an array of selected computed-field names, not a
    // Record keyed by name. Set the whole array so it stays consistent with
    // the schema and the CheckboxesField. Writing `computed_fields.<name>`
    // turned it into an object and later serialized as array indices
    // ("0", "1", ...), corrupting the saved permission.
    if (computedFields?.length) {
      setValue(
        'computed_fields',
        toggleAllOn
          ? computedFields.filter(
              (fieldName) => !tableReturningComputedFields.includes(fieldName),
            )
          : [],
      );
    }
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
                options={tableComputedFields.map((fieldName) => {
                  const returnsTable =
                    tableReturningComputedFields.includes(fieldName);
                  return {
                    label: (
                      <Flex align="center" gap="1">
                        <Text className="italic">{fieldName}</Text>
                        {returnsTable && (
                          <IconTooltip message="This computed field returns a table. Its select permissions are derived from the permissions on the returning table and cannot be set here." />
                        )}
                      </Flex>
                    ),
                    value: fieldName,
                    disabled: disabled || returnsTable,
                    title: disabled ? 'Set a row permission first' : undefined,
                  };
                })}
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
