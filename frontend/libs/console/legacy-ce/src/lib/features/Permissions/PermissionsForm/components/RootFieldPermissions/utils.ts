import { TableColumn } from '@hasura/metadata/data-source';
import {
  QueryRootPermissionTypes,
  SubscriptionRootPermissionTypes,
} from './types';
import {
  queryRootPermissionFields,
  subscriptionRootPermissionFields,
} from '@hasura/shared/types';

export type SectionLabelProps = {
  subscriptionRootPermissions: SubscriptionRootPermissionTypes;
  queryRootPermissions: QueryRootPermissionTypes;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  isSubscriptionStreamingEnabled: boolean | undefined;
};

export const getSectionStatusLabel = ({
  subscriptionRootPermissions,
  queryRootPermissions,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  isSubscriptionStreamingEnabled,
}: SectionLabelProps) => {
  if (subscriptionRootPermissions === null && queryRootPermissions === null)
    return '  - all enabled';

  if (
    subscriptionRootPermissions?.length === 0 &&
    queryRootPermissions?.length === 0
  )
    return '  - all disabled';

  let currentAmountOfAvailablePermission =
    queryRootPermissionFields.length + subscriptionRootPermissionFields.length;

  if (!hasEnabledAggregations) {
    // exists on both query and subscription
    currentAmountOfAvailablePermission -= 2;
  }

  if (!hasSelectedPrimaryKeys) {
    // exists on both query and subscription
    currentAmountOfAvailablePermission -= 2;
  }

  if (!isSubscriptionStreamingEnabled) {
    // exists only on subscription
    currentAmountOfAvailablePermission -= 1;
  }

  const amountOfSelectedPermissions =
    (queryRootPermissions?.length || 0) +
    (subscriptionRootPermissions?.length || 0);

  if (currentAmountOfAvailablePermission === amountOfSelectedPermissions) {
    return '  - all enabled';
  }

  return '  - partially enabled';
};

export type PermissionCheckboxStateArg = {
  permission: string;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  isSubscriptionStreamingEnabled: boolean | undefined;
};

type SelectByPkCheckboxStateArgs = {
  hasSelectedPrimaryKeys: boolean;
};

export const getSelectByPkCheckboxState = ({
  hasSelectedPrimaryKeys,
}: SelectByPkCheckboxStateArgs) => {
  return {
    disabled: !hasSelectedPrimaryKeys,
    title: !hasSelectedPrimaryKeys
      ? 'Allow access to the table primary key column(s) first'
      : '',
  };
};

type SelectStreamCheckboxStateArg = {
  isSubscriptionStreamingEnabled: boolean | undefined;
};

export const getSelectStreamCheckboxState = ({
  isSubscriptionStreamingEnabled,
}: SelectStreamCheckboxStateArg) => ({
  disabled: !isSubscriptionStreamingEnabled,
  title: !isSubscriptionStreamingEnabled
    ? 'Enable the streaming subscriptions experimental feature first'
    : '',
});

type SelectAggregateCheckboxStateArg = {
  hasEnabledAggregations: boolean;
};

export const getSelectAggregateCheckboxState = ({
  hasEnabledAggregations,
}: SelectAggregateCheckboxStateArg) => {
  return {
    disabled: !hasEnabledAggregations,
    title: !hasEnabledAggregations
      ? 'Enable aggregation queries permissions first'
      : '',
  };
};

export const getPermissionCheckboxState = ({
  permission,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  isSubscriptionStreamingEnabled,
}: PermissionCheckboxStateArg) => {
  switch (permission) {
    case 'select_by_pk':
      return getSelectByPkCheckboxState({
        hasSelectedPrimaryKeys,
      });

    case 'select_stream':
      return getSelectStreamCheckboxState({
        isSubscriptionStreamingEnabled,
      });

    case 'select_aggregate':
      return getSelectAggregateCheckboxState({
        hasEnabledAggregations,
      });
    default:
      return {
        disabled: false,
      };
  }
};

export const hasSelectedPrimaryKey = (
  selectedColumns: Record<string, boolean | undefined>,
  columns: TableColumn[] | undefined,
) => {
  if (!columns?.length) {
    return false;
  }

  return columns.some((column) => {
    const isPrimaryKey = column.isPrimaryKey;
    const colName = column.name;
    const hasPickedColumn = selectedColumns[colName];
    return hasPickedColumn && isPrimaryKey;
  });
};
