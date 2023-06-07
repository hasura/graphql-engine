import React, { useState } from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { Button } from '../../../../new-components/Button';
import { Collapse } from '../../../../new-components/deprecated';
import { TableColumn } from '../../../DataSource';
import { useListAllTableColumns } from '../../../Data';
import { PermissionsConfirmationModal } from './RootFieldPermissions/PermissionsConfirmationModal';
import { getEdForm } from '../../../../components/Services/Data/utils';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { QueryType } from '../../types';
import { isPermissionModalDisabled } from '../utils/getPermissionModalStatus';

import {
  getPermissionsModalTitle,
  getPermissionsModalDescription,
} from './RootFieldPermissions/PermissionsConfirmationModal.utils';
import {
  SubscriptionRootPermissionType,
  QueryRootPermissionType,
} from './RootFieldPermissions/types';

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
  queryType: QueryType;
  roleName: string;
  columns?: string[];
  table: unknown;
  dataSourceName: string;
}

const useStatus = (disabled: boolean) => {
  const { control } = useFormContext();
  const formColumns = useWatch({ control, name: 'columns' });

  if (!formColumns) {
    return { data: '', isError: false };
  }

  const columnValues = Object.values(formColumns);
  const selectedColumns = columnValues.filter(value => !!value);

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
  queryRootFields: QueryRootPermissionType,
  subscriptionRootFields: SubscriptionRootPermissionType
) => {
  const primaryKeys = tableColumns
    ?.filter(column => column.isPrimaryKey)
    ?.map(column => column.name);
  const pkRootFieldsAreSelected =
    queryRootFields?.includes('select_by_pk') ||
    subscriptionRootFields?.includes('select_by_pk');
  return (
    selectedColumns[fieldName] &&
    pkRootFieldsAreSelected &&
    primaryKeys.includes(fieldName)
  );
};

// @todo
// this hasn't been fully implemented, it still needs computed columns adding
export const ColumnPermissionsSection: React.FC<
  ColumnPermissionsSectionProps
> = ({ roleName, queryType, columns, table, dataSourceName }) => {
  const { setValue, watch } = useFormContext();
  const [showConfirmation, setShowConfirmationModal] = useState<string | null>(
    null
  );
  watch();

  const [selectedColumns, queryRootFields, subscriptionRootFields] = watch([
    'columns',
    'query_root_fields',
    'subscription_root_fields',
  ]);

  // if no row permissions are selected selection should be disabled
  const disabled = useIsDisabled(queryType);

  const { data: status, isError } = useStatus(disabled);

  const { columns: tableColumns } = useListAllTableColumns(
    dataSourceName,
    table
  );

  const onClick = () => {
    columns?.forEach(column => {
      const toggleAllOn = status !== 'All columns';
      // if status is not all columns: toggle all on
      // otherwise toggle all off
      setValue(`columns.${column}`, toggleAllOn);
    });
  };

  if (isError) {
    return <div>Error loading column permission data</div>;
  }

  const handleUpdate = (fieldName: string) => {
    setValue(
      'query_root_fields',
      queryRootFields.filter((field: string) => field !== 'select_by_pk')
    );
    setValue(
      'subscription_root_fields',
      subscriptionRootFields.filter((field: string) => field !== 'select_by_pk')
    );
    setValue(`columns.${fieldName}`, !selectedColumns[fieldName]);
  };

  const permissionsModalTitle = getPermissionsModalTitle({
    scenario: 'pks',
    role: roleName,
    primaryKeyColumns: tableColumns
      ?.filter(column => column.isPrimaryKey)
      ?.map(column => column.name)
      ?.join(','),
  });

  const permissionsModalDescription = getPermissionsModalDescription('pks');

  return (
    <>
      <Collapse defaultOpen={!disabled}>
        <Collapse.Header
          title={`Column ${queryType} permissions`}
          tooltip={`Choose columns allowed to be ${getEdForm(queryType)}`}
          status={status}
          disabled={disabled}
          disabledMessage="Set row permissions first"
        />
        <Collapse.Content>
          <div
            title={disabled ? 'Set row permissions first' : ''}
            className="grid gap-2"
          >
            <div className="flex gap-2 items-center">
              <p>
                Allow role <strong>{roleName}</strong>{' '}
                {getAccessText(queryType)}
                &nbsp;
                <strong>columns</strong>:
              </p>
            </div>
            <fieldset className="flex gap-4 flex-wrap">
              {columns?.map(fieldName => (
                <label key={fieldName} className="flex gap-2 items-center">
                  <input
                    type="checkbox"
                    title={disabled ? 'Set a row permission first' : ''}
                    disabled={disabled}
                    style={{ marginTop: '0px !important' }}
                    className="rounded shadow-sm border border-gray-300 hover:border-gray-400 focus:ring-yellow-400"
                    checked={selectedColumns[fieldName]}
                    onChange={() => {
                      const hideModal = isPermissionModalDisabled();
                      if (
                        !hideModal &&
                        !showConfirmation &&
                        checkIfConfirmationIsNeeded(
                          fieldName,
                          tableColumns,
                          selectedColumns,
                          queryRootFields,
                          subscriptionRootFields
                        )
                      ) {
                        setShowConfirmationModal(fieldName);
                        return;
                      }
                      setValue(
                        `columns.${fieldName}`,
                        !selectedColumns[fieldName]
                      );
                    }}
                  />
                  <i>{fieldName}</i>
                </label>
              ))}
              <Button
                type="button"
                size="sm"
                title={disabled ? 'Set a row permission first' : ''}
                disabled={disabled}
                onClick={onClick}
                data-test="toggle-all-col-btn"
              >
                Toggle All
              </Button>
            </fieldset>
          </div>
          {/* {getExternalTablePermissionsMsg()} */}
        </Collapse.Content>
      </Collapse>
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
