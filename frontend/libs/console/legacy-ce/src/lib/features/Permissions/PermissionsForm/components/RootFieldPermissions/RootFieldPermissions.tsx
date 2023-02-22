import React from 'react';
import { useFormContext } from 'react-hook-form';
import clsx from 'clsx';
import { Collapse } from '../../../../../new-components/deprecated';
import { Switch } from '../../../../../new-components/Switch';
import { Table } from '../../../../hasura-metadata-types';
import { useListAllTableColumns } from '../../../../Data';
import {
  getSectionStatusLabel,
  hasSelectedPrimaryKey as hasSelectedPrimaryKeyFinder,
} from './utils';
import { SelectPermissionsRow } from './SelectPermissionsRow';

import {
  QueryRootPermissionType,
  SubscriptionRootPermissionType,
  PermissionRootTypes,
} from './types';
import { useRootFieldPermissions } from './hooks/useRootFieldPermissions';
import { useSourceSupportStreaming } from '../../hooks/useSourceSupportStreaming';
import { IconTooltip } from '../../../../../new-components/Tooltip/IconTooltip';

export type RootKeyValues = 'query_root_fields' | 'subscription_root_fields';

export const QUERY_ROOT_VALUES = 'query_root_fields';
export const SUBSCRIPTION_ROOT_VALUES = 'subscription_root_fields';

export const queryRootPermissionFields: QueryRootPermissionType[] = [
  'select',
  'select_by_pk',
  'select_aggregate',
];

export const subscriptionRootPermissionFields: SubscriptionRootPermissionType[] =
  ['select', 'select_by_pk', 'select_aggregate', 'select_stream'];

const QueryRootFieldDescription = () => (
  <div>
    Allow the following root fields under the <b>Query root field</b>
  </div>
);

const SubscriptionRootFieldDescription = () => (
  <div>
    Allow the following root fields under the <b>Subscription root field</b>
  </div>
);

export interface ColumnPermissionsSectionProps {
  columns?: string[];
  filterType: string;
  table: Table;
  dataSourceName: string;
}

export const ColumnRootFieldPermissions: React.FC<
  ColumnPermissionsSectionProps
> = ({ dataSourceName, table, filterType }) => {
  const { watch, setValue } = useFormContext();

  const [
    hasEnabledAggregations,
    selectedColumns,
    queryRootFields,
    subscriptionRootFields,
  ] = watch([
    'aggregationEnabled',
    'columns',
    'query_root_fields',
    'subscription_root_fields',
  ]);
  const disabled = filterType === 'none';
  const { columns: tableColumns } = useListAllTableColumns(
    dataSourceName,
    table
  );

  const hasSelectedPrimaryKeys = hasSelectedPrimaryKeyFinder(
    selectedColumns,
    tableColumns
  );

  const updateFormValues = (key: RootKeyValues, value: PermissionRootTypes) => {
    setValue(key, value);
  };

  const rootFieldPermissions = useRootFieldPermissions({
    queryRootFields,
    subscriptionRootFields,
    hasEnabledAggregations,
    hasSelectedPrimaryKeys,
    updateFormValues,
  });

  const {
    isSubscriptionStreamingEnabled,
    onEnableSectionSwitchChange,
    onToggleAll,
    isRootPermissionsSwitchedOn,
    onUpdatePermission,
  } = rootFieldPermissions;

  const supportsStreaming = useSourceSupportStreaming(dataSourceName);
  const getFilteredSubscriptionRootPermissionFields = (
    fields: SubscriptionRootPermissionType[]
  ) => {
    if (!supportsStreaming)
      return fields.filter(field => field !== 'select_stream');
    return fields;
  };

  const bodyTitle = disabled ? 'Set row permissions first' : '';

  return (
    <Collapse defaultOpen={!disabled}>
      <Collapse.Header
        title="Root field permissions"
        tooltip="Choose root fields to be added under the query and subscription root fields."
        status={getSectionStatusLabel({
          queryRootPermissions: queryRootFields,
          subscriptionRootPermissions: subscriptionRootFields,
          hasEnabledAggregations,
          hasSelectedPrimaryKeys,
          isSubscriptionStreamingEnabled,
        })}
        disabledMessage="Set row permissions first"
      />
      <Collapse.Content>
        <div title={bodyTitle}>
          <div
            className={`px-md mb-xs flex items-center ${clsx(
              disabled && `opacity-70 pointer-events-none`
            )}`}
          >
            <Switch
              checked={isRootPermissionsSwitchedOn}
              onCheckedChange={onEnableSectionSwitchChange}
            />
            <div className="mx-xs">
              Enable GraphQL root field visibility customization.
            </div>
            <IconTooltip
              message={
                'By enabling this you can customize the root field permissions. When this switch is turned off, all values are enabled by default.'
              }
            />
          </div>
        </div>
        <div
          className={`px-md ${clsx(!isRootPermissionsSwitchedOn && 'hidden')}`}
        >
          <SelectPermissionsRow
            currentPermissions={queryRootFields}
            description={<QueryRootFieldDescription />}
            hasEnabledAggregations={hasEnabledAggregations}
            hasSelectedPrimaryKeys={hasSelectedPrimaryKeys}
            isSubscriptionStreamingEnabled={isSubscriptionStreamingEnabled}
            permissionFields={queryRootPermissionFields}
            permissionType={QUERY_ROOT_VALUES}
            onToggleAll={() => onToggleAll(QUERY_ROOT_VALUES, queryRootFields)}
            onUpdate={onUpdatePermission}
          />
          <SelectPermissionsRow
            currentPermissions={subscriptionRootFields}
            description={<SubscriptionRootFieldDescription />}
            hasEnabledAggregations={hasEnabledAggregations}
            hasSelectedPrimaryKeys={hasSelectedPrimaryKeys}
            isSubscriptionStreamingEnabled={isSubscriptionStreamingEnabled}
            permissionFields={getFilteredSubscriptionRootPermissionFields(
              subscriptionRootPermissionFields
            )}
            permissionType={SUBSCRIPTION_ROOT_VALUES}
            onToggleAll={() =>
              onToggleAll(SUBSCRIPTION_ROOT_VALUES, subscriptionRootFields)
            }
            onUpdate={onUpdatePermission}
          />
        </div>
      </Collapse.Content>
    </Collapse>
  );
};

export default ColumnRootFieldPermissions;
