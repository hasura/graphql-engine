import { useServerConfig } from '../../../../../hooks';
import {
  RootKeyValues,
  SUBSCRIPTION_ROOT_VALUES,
  QUERY_ROOT_VALUES,
  subscriptionRootPermissionFields,
  queryRootPermissionFields,
} from './RootFieldPermissions';
import {
  PermissionRootType,
  PermissionRootTypes,
  RootFieldPermissionsType,
  SubscriptionRootPermissionTypes,
} from './types';

type Props = Pick<
  RootFieldPermissionsType,
  | 'hasEnabledAggregations'
  | 'hasSelectedPrimaryKeys'
  | 'onSubmitUpdate'
  | 'queryRootPermissions'
  | 'subscriptionRootPermissions'
>;

export const useRootFieldPermissions = ({
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  onSubmitUpdate,
  queryRootPermissions,
  subscriptionRootPermissions,
}: Props) => {
  const { data: configData } = useServerConfig();
  const isSubscriptionStreamingEnabled =
    !!configData?.experimental_features.includes('streaming_subscriptions');

  const isRootPermissionsSwitchedOn =
    subscriptionRootPermissions !== null && queryRootPermissions !== null;

  const onPermissionUpdate = (
    key: RootKeyValues,
    permission: PermissionRootType,
    currentPermissionArray: PermissionRootTypes
  ) => {
    const containsString = currentPermissionArray?.includes(permission);
    if (containsString || !currentPermissionArray) {
      const toDispatch = currentPermissionArray?.filter(
        queryPermission => queryPermission !== permission
      );
      onSubmitUpdate(key, toDispatch);
      return;
    }
    onSubmitUpdate(
      key,
      [...currentPermissionArray, permission].filter(Boolean)
    );
  };

  const onEnableSection = (
    key: RootKeyValues,
    permissionTypeFields: PermissionRootTypes
  ) => {
    let newState = permissionTypeFields;

    if (!hasEnabledAggregations) {
      newState = newState.filter(
        (permission: string) => permission !== 'select_aggregate'
      );
    }

    if (!hasSelectedPrimaryKeys) {
      newState = newState.filter(
        (permission: string) => permission !== 'select_by_pk'
      );
    }

    if (key === SUBSCRIPTION_ROOT_VALUES && !isSubscriptionStreamingEnabled) {
      newState = newState.filter(
        (permission: string) => permission !== 'select_stream'
      );
    }

    onSubmitUpdate(key, newState);
  };

  const onToggleAll = (
    key: RootKeyValues,
    currentPermissionArray: PermissionRootTypes
  ) => {
    if (currentPermissionArray?.length > 0) {
      return onSubmitUpdate(key, []);
    }
    const toToggle: SubscriptionRootPermissionTypes = ['select'];
    if (key === SUBSCRIPTION_ROOT_VALUES && isSubscriptionStreamingEnabled) {
      toToggle.push('select_stream');
    }
    if (hasEnabledAggregations) toToggle.push('select_aggregate');
    if (hasSelectedPrimaryKeys) toToggle.push('select_by_pk');

    onSubmitUpdate(key, toToggle);
  };

  const onEnableSectionSwitchChange = () => {
    if (isRootPermissionsSwitchedOn) {
      onSubmitUpdate(SUBSCRIPTION_ROOT_VALUES, null);
      onSubmitUpdate(QUERY_ROOT_VALUES, null);
      return;
    }

    onEnableSection(SUBSCRIPTION_ROOT_VALUES, subscriptionRootPermissionFields);
    onEnableSection(QUERY_ROOT_VALUES, queryRootPermissionFields);
  };

  return {
    isSubscriptionStreamingEnabled,
    onEnableSectionSwitchChange,
    onPermissionUpdate,
    onToggleAll,
    isRootPermissionsSwitchedOn,
  };
};
