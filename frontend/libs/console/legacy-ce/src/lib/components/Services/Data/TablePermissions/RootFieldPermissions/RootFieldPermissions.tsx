import React from 'react';
import { OverlayTrigger, Tooltip } from 'react-bootstrap';
import { FaQuestionCircle } from 'react-icons/fa';
import { Switch } from '../../../../../new-components/Switch';
import clsx from 'clsx';

import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import { SelectPermissionsRow } from './SelectPermissionsRow';
import { RootFieldPermissionsType } from './types';
import { RootFieldsPermissionsTitle } from './RootFieldsPermissionsTitle';
import { useRootFieldPermissions } from './useRootFieldPermissions';
import {
  subscriptionRootPermissionFields,
  queryRootPermissionFields,
} from './constants';

export type RootKeyValues = 'query_root_values' | 'subscription_root_values';

export { subscriptionRootPermissionFields, queryRootPermissionFields };

export const QUERY_ROOT_VALUES = 'query_root_values';
export const SUBSCRIPTION_ROOT_VALUES = 'subscription_root_values';

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

export const RootFieldPermissions: React.FC<RootFieldPermissionsType> = ({
  subscriptionRootPermissions,
  queryRootPermissions,
  onSubmitUpdate,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  disabled,
}) => {
  const rootFieldPermissions = useRootFieldPermissions({
    hasEnabledAggregations,
    hasSelectedPrimaryKeys,
    onSubmitUpdate,
    queryRootPermissions,
    subscriptionRootPermissions,
  });

  const {
    isSubscriptionStreamingEnabled,
    onEnableSectionSwitchChange,
    onPermissionUpdate,
    onToggleAll,
    isRootPermissionsSwitchedOn,
  } = rootFieldPermissions;

  const bodyTitle = disabled ? 'Set row permissions first' : '';

  return (
    <div className="flex flex-col">
      <CollapsibleToggle
        title={
          <RootFieldsPermissionsTitle
            subscriptionRootPermissions={subscriptionRootPermissions}
            queryRootPermissions={queryRootPermissions}
            hasEnabledAggregations={hasEnabledAggregations}
            hasSelectedPrimaryKeys={hasSelectedPrimaryKeys}
            isSubscriptionStreamingEnabled={isSubscriptionStreamingEnabled}
          />
        }
        useDefaultTitleStyle
        testId="toggle-root-query-permissions"
      >
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
            <OverlayTrigger
              placement="right"
              overlay={
                <Tooltip id="tooltip-root-permissions-switch">
                  By enabling this you can customize the root field permissions.
                  When this switch is turned off, all values are enabled by
                  default.
                </Tooltip>
              }
            >
              <FaQuestionCircle aria-hidden="true" />
            </OverlayTrigger>
          </div>
          <div
            className={`px-md ${clsx(
              !isRootPermissionsSwitchedOn && 'hidden'
            )}`}
          >
            <SelectPermissionsRow
              currentPermissions={queryRootPermissions}
              description={<QueryRootFieldDescription />}
              hasEnabledAggregations={hasEnabledAggregations}
              hasSelectedPrimaryKeys={hasSelectedPrimaryKeys}
              isSubscriptionStreamingEnabled={isSubscriptionStreamingEnabled}
              permissionFields={queryRootPermissionFields}
              permissionType={QUERY_ROOT_VALUES}
              onToggleAll={() =>
                onToggleAll(QUERY_ROOT_VALUES, queryRootPermissions)
              }
              onUpdate={onPermissionUpdate}
            />
            <SelectPermissionsRow
              currentPermissions={subscriptionRootPermissions}
              description={<SubscriptionRootFieldDescription />}
              hasEnabledAggregations={hasEnabledAggregations}
              hasSelectedPrimaryKeys={hasSelectedPrimaryKeys}
              isSubscriptionStreamingEnabled={isSubscriptionStreamingEnabled}
              permissionFields={subscriptionRootPermissionFields}
              permissionType={SUBSCRIPTION_ROOT_VALUES}
              onToggleAll={() =>
                onToggleAll(
                  SUBSCRIPTION_ROOT_VALUES,
                  subscriptionRootPermissions
                )
              }
              onUpdate={onPermissionUpdate}
            />
          </div>
        </div>
      </CollapsibleToggle>
    </div>
  );
};
