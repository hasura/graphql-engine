import React from 'react';
import { useFormContext } from 'react-hook-form';
import clsx from 'clsx';
import {
  QualifiedDataSource,
  queryRootPermissionFields,
  subscriptionRootPermissionFields,
  SubscriptionRootPermissionType,
  Table,
} from '@hasura/shared/types';
import {
  getSectionStatusLabel,
  hasSelectedPrimaryKey as hasSelectedPrimaryKeyFinder,
} from './utils';
import { SelectPermissionsRow } from './SelectPermissionsRow';
import { PermissionRootTypes } from './types';
import { useRootFieldPermissions } from './hooks/useRootFieldPermissions';
import {
  Collapsible,
  CollapsibleHeader,
  Switch,
  IconTooltip,
} from '@hasura/shared/ui';
import { useTableColumns } from '@hasura/metadata/data-source';
import { Flex } from '@radix-ui/themes';
import { PermissionsSchema } from '../../../schema';

export type RootKeyValues = 'query_root_fields' | 'subscription_root_fields';

export const QUERY_ROOT_VALUES = 'query_root_fields';
export const SUBSCRIPTION_ROOT_VALUES = 'subscription_root_fields';

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
  source: QualifiedDataSource;
}

export const ColumnRootFieldPermissions: React.FC<
  ColumnPermissionsSectionProps
> = ({ source, table, filterType }) => {
  const { watch, setValue } = useFormContext<PermissionsSchema>();

  const [
    hasEnabledAggregations,
    customRootFieldEnabled,
    selectedColumns,
    queryRootFields,
    subscriptionRootFields,
  ] = watch([
    'aggregationEnabled',
    'customRootFieldEnabled',
    'columns',
    'query_root_fields',
    'subscription_root_fields',
  ]);
  const disabled = filterType === 'none';
  const { data: tableColumns } = useTableColumns({ source, table });

  const hasSelectedPrimaryKeys = hasSelectedPrimaryKeyFinder(
    selectedColumns,
    tableColumns?.columns ?? [],
  );

  const updateFormValues = (key: RootKeyValues, value: PermissionRootTypes) => {
    setValue(key, value);
  };

  const { isSubscriptionEnabled, onEnableSectionSwitchChange, onToggleAll } =
    useRootFieldPermissions({
      source,
      customRootFieldEnabled,
      hasEnabledAggregations,
      hasSelectedPrimaryKeys,
      updateFormValues,
    });

  const getFilteredSubscriptionRootPermissionFields = (
    fields: readonly SubscriptionRootPermissionType[],
  ) => {
    if (!isSubscriptionEnabled)
      return fields.filter((field) => field !== 'select_stream');
    return [...fields];
  };

  const bodyTitle = disabled ? 'Set row permissions first' : '';

  return (
    <Collapsible
      defaultOpen={!disabled}
      disabled={disabled}
      triggerChildren={
        <CollapsibleHeader
          title="Root field permissions"
          tooltip={
            disabled
              ? 'Set row permissions first'
              : 'Choose root fields to be added under the query and subscription root fields.'
          }
          status={getSectionStatusLabel({
            queryRootPermissions: queryRootFields,
            subscriptionRootPermissions: subscriptionRootFields,
            hasEnabledAggregations,
            hasSelectedPrimaryKeys,
            isSubscriptionStreamingEnabled: isSubscriptionEnabled,
          })}
        />
      }
    >
      <div title={bodyTitle}>
        <Flex
          align="center"
          gap="2"
          className={`m4-2 flex items-center ${clsx(
            disabled && `opacity-70 pointer-events-none`,
          )}`}
        >
          <Switch
            value={customRootFieldEnabled}
            onChange={(value) => {
              setValue('customRootFieldEnabled', value);
              onEnableSectionSwitchChange(value);
            }}
          >
            Enable GraphQL root field visibility customization.
          </Switch>
          <IconTooltip
            message={
              'By enabling this you can customize the root field permissions. When this switch is turned off, all values are enabled by default.'
            }
          />
        </Flex>
      </div>
      {customRootFieldEnabled && (
        <div>
          <SelectPermissionsRow
            name={QUERY_ROOT_VALUES}
            description={<QueryRootFieldDescription />}
            hasEnabledAggregations={hasEnabledAggregations}
            hasSelectedPrimaryKeys={hasSelectedPrimaryKeys}
            isSubscriptionStreamingEnabled={isSubscriptionEnabled}
            permissionFields={[...queryRootPermissionFields]}
            onToggleAll={() => onToggleAll(QUERY_ROOT_VALUES, queryRootFields)}
          />
          <SelectPermissionsRow
            description={<SubscriptionRootFieldDescription />}
            hasEnabledAggregations={hasEnabledAggregations}
            hasSelectedPrimaryKeys={hasSelectedPrimaryKeys}
            isSubscriptionStreamingEnabled={isSubscriptionEnabled}
            permissionFields={getFilteredSubscriptionRootPermissionFields(
              subscriptionRootPermissionFields,
            )}
            name={SUBSCRIPTION_ROOT_VALUES}
            onToggleAll={() =>
              onToggleAll(SUBSCRIPTION_ROOT_VALUES, subscriptionRootFields)
            }
          />
        </div>
      )}
    </Collapsible>
  );
};

export default ColumnRootFieldPermissions;
