import { useDriverCapabilities } from '@hasura/metadata/data-source';
import {
  RootKeyValues,
  SUBSCRIPTION_ROOT_VALUES,
  QUERY_ROOT_VALUES,
} from '../RootFieldPermissions';
import { SubscriptionRootPermissionTypes, PermissionRootTypes } from '../types';
import {
  QualifiedDataSource,
  queryRootPermissionFields,
  subscriptionRootPermissionFields,
} from '@hasura/shared/types';

export type RootFieldPermissionsType = {
  source: QualifiedDataSource;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  customRootFieldEnabled: boolean;
  updateFormValues: (key: RootKeyValues, value: PermissionRootTypes) => void;
};

export const useRootFieldPermissions = ({
  source,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  updateFormValues,
}: RootFieldPermissionsType) => {
  const { data: capabilities } = useDriverCapabilities({
    source,
  });

  const isSubscriptionEnabled = Boolean(capabilities?.subscriptions);

  const onEnableSection = (
    key: RootKeyValues,
    permissionTypeFields: PermissionRootTypes,
  ) => {
    if (permissionTypeFields === null) return;

    let newState = permissionTypeFields;

    if (!hasEnabledAggregations) {
      newState = newState.filter(
        (permission: string) => permission !== 'select_aggregate',
      );
    }

    if (!hasSelectedPrimaryKeys) {
      newState = newState.filter(
        (permission: string) => permission !== 'select_by_pk',
      );
    }

    if (!isSubscriptionEnabled) {
      newState = newState.filter(
        (permission: string) => permission !== 'select_stream',
      );
    }

    updateFormValues(key, newState);
  };

  const onToggleAll = (
    key: RootKeyValues,
    currentPermissionArray: PermissionRootTypes,
  ) => {
    if (currentPermissionArray && currentPermissionArray?.length > 0) {
      return updateFormValues(key, []);
    }
    const toToggle: SubscriptionRootPermissionTypes = ['select'];
    if (key === SUBSCRIPTION_ROOT_VALUES && isSubscriptionEnabled) {
      toToggle.push('select_stream');
    }
    if (hasEnabledAggregations) toToggle.push('select_aggregate');
    if (hasSelectedPrimaryKeys) toToggle.push('select_by_pk');

    updateFormValues(key, toToggle);
  };

  const onEnableSectionSwitchChange = (enabled: boolean) => {
    if (!enabled) {
      updateFormValues(SUBSCRIPTION_ROOT_VALUES, []);
      updateFormValues(QUERY_ROOT_VALUES, []);
      return;
    }

    onEnableSection(SUBSCRIPTION_ROOT_VALUES, [
      ...subscriptionRootPermissionFields,
    ]);
    onEnableSection(QUERY_ROOT_VALUES, [...queryRootPermissionFields]);
  };

  return {
    isSubscriptionEnabled,
    onEnableSectionSwitchChange,
    onToggleAll,
  };
};
