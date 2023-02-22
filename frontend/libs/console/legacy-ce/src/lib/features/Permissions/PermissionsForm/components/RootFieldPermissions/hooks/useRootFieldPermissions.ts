import { useServerConfig } from '../../../../../../hooks';
import {
  RootKeyValues,
  SUBSCRIPTION_ROOT_VALUES,
  QUERY_ROOT_VALUES,
  subscriptionRootPermissionFields,
  queryRootPermissionFields,
} from '../RootFieldPermissions';
import {
  PermissionRootType,
  RootFieldPermissionsType,
  SubscriptionRootPermissionTypes,
  PermissionRootTypes,
  CombinedPermissionRootTypes,
} from '../types';

type Props = RootFieldPermissionsType;

export const useRootFieldPermissions = ({
  queryRootFields,
  subscriptionRootFields,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  updateFormValues,
}: Props) => {
  const { data: configData } = useServerConfig();
  const isSubscriptionStreamingEnabled =
    !!configData?.experimental_features.includes('streaming_subscriptions');

  const isRootPermissionsSwitchedOn =
    queryRootFields !== null && subscriptionRootFields !== null;

  const onUpdatePermission = (
    key: RootKeyValues,
    permission: PermissionRootType,
    currentPermissionArray: CombinedPermissionRootTypes
  ) => {
    const containsString = currentPermissionArray?.includes(permission);
    if (containsString || !currentPermissionArray) {
      const newPermissionArray = currentPermissionArray?.filter(
        (queryPermission: string) => queryPermission !== permission
      );
      if (newPermissionArray) updateFormValues(key, newPermissionArray);
      return;
    }

    updateFormValues(
      key,
      [...currentPermissionArray, permission].filter(Boolean)
    );
  };

  const onEnableSection = (
    key: RootKeyValues,
    permissionTypeFields: PermissionRootTypes
  ) => {
    if (permissionTypeFields === null) return;

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

    updateFormValues(key, newState);
  };

  const onToggleAll = (
    key: RootKeyValues,
    currentPermissionArray: PermissionRootTypes
  ) => {
    if (currentPermissionArray && currentPermissionArray?.length > 0) {
      return updateFormValues(key, []);
    }
    const toToggle: SubscriptionRootPermissionTypes = ['select'];
    if (key === SUBSCRIPTION_ROOT_VALUES && isSubscriptionStreamingEnabled) {
      toToggle.push('select_stream');
    }
    if (hasEnabledAggregations) toToggle.push('select_aggregate');
    if (hasSelectedPrimaryKeys) toToggle.push('select_by_pk');

    updateFormValues(key, toToggle);
  };

  const onEnableSectionSwitchChange = () => {
    if (isRootPermissionsSwitchedOn) {
      updateFormValues(SUBSCRIPTION_ROOT_VALUES, null);
      updateFormValues(QUERY_ROOT_VALUES, null);
      return;
    }

    onEnableSection(SUBSCRIPTION_ROOT_VALUES, subscriptionRootPermissionFields);
    onEnableSection(QUERY_ROOT_VALUES, queryRootPermissionFields);
  };

  return {
    isSubscriptionStreamingEnabled,
    onEnableSectionSwitchChange,
    onToggleAll,
    onUpdatePermission,
    isRootPermissionsSwitchedOn,
  };
};
