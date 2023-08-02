import { TableColumn } from '../../../../DataSource';
import {
  queryRootPermissionFields,
  subscriptionRootPermissionFields,
} from './RootFieldPermissions';
import {
  QueryRootPermissionTypes,
  SubscriptionRootPermissionTypes,
} from './types';

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

type CheckboxPermissionStateProps = {
  checked: boolean;
  disabled: boolean;
  title?: string;
};

export type PermissionCheckboxStateArg = {
  permission: string;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  isSubscriptionStreamingEnabled: boolean | undefined;
  rootPermissions: string[] | null;
};

type SelectByPkCheckboxStateArgs = {
  hasSelectedPrimaryKeys: boolean;
  rootPermissions: string[];
  permission: string;
};

export const getSelectByPkCheckboxState = ({
  hasSelectedPrimaryKeys,
  rootPermissions,
  permission,
}: SelectByPkCheckboxStateArgs): CheckboxPermissionStateProps => {
  const getPkCheckedState = () => {
    if (!hasSelectedPrimaryKeys) return false;
    if (rootPermissions?.includes(permission)) return true;
    return false;
  };

  return {
    checked: getPkCheckedState(),
    disabled: !hasSelectedPrimaryKeys,
    title: !hasSelectedPrimaryKeys
      ? 'Allow access to the table primary key column(s) first'
      : '',
  };
};

type SelectStreamCheckboxStateArg = {
  rootPermissions: string[];
  permission: string;
  isSubscriptionStreamingEnabled: boolean | undefined;
};

export const getSelectStreamCheckboxState = ({
  rootPermissions,
  permission,
  isSubscriptionStreamingEnabled,
}: SelectStreamCheckboxStateArg): CheckboxPermissionStateProps => ({
  checked: rootPermissions?.includes(permission),
  disabled: !isSubscriptionStreamingEnabled,
  title: !isSubscriptionStreamingEnabled
    ? 'Enable the streaming subscriptions experimental feature first'
    : '',
});

type SelectAggregateCheckboxStateArg = {
  hasEnabledAggregations: boolean;
  rootPermissions: string[];
  permission: string;
};
export const getSelectAggregateCheckboxState = ({
  hasEnabledAggregations,
  rootPermissions,
  permission,
}: SelectAggregateCheckboxStateArg) => {
  const getAggregationCheckedState = () => {
    if (!hasEnabledAggregations) return false;
    if (rootPermissions?.includes(permission)) return true;
    return false;
  };
  return {
    checked: getAggregationCheckedState(),
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
  rootPermissions,
}: PermissionCheckboxStateArg): CheckboxPermissionStateProps => {
  if (rootPermissions === null)
    return {
      disabled: true,
      checked: true,
    };

  switch (permission) {
    case 'select_by_pk':
      return getSelectByPkCheckboxState({
        hasSelectedPrimaryKeys,
        rootPermissions,
        permission,
      });

    case 'select_stream':
      return getSelectStreamCheckboxState({
        rootPermissions,
        permission,
        isSubscriptionStreamingEnabled,
      });

    case 'select_aggregate':
      return getSelectAggregateCheckboxState({
        hasEnabledAggregations,
        rootPermissions,
        permission,
      });
    default:
      return {
        disabled: false,
        checked: rootPermissions?.includes(permission),
      };
  }
};

export const hasSelectedPrimaryKey = (
  selectedColumns: Record<string, boolean | undefined>,
  columns: TableColumn[]
) => {
  return !!columns.find(column => {
    const isPrimaryKey = column.isPrimaryKey;
    const colName = column.name;
    const hasPickedColumn = selectedColumns[colName];
    return hasPickedColumn && isPrimaryKey;
  });
};
