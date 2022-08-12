import React from 'react';
import { OverlayTrigger, Tooltip } from 'react-bootstrap';
import { FaQuestionCircle } from 'react-icons/fa';
import {
  QueryRootPermissionTypes,
  SubscriptionRootPermissionTypes,
} from './types';
import { getSectionStatusLabel } from './utils';

type Props = {
  subscriptionRootPermissions: SubscriptionRootPermissionTypes;
  queryRootPermissions: QueryRootPermissionTypes;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  isSubscriptionStreamingEnabled: boolean;
};

export const RootFieldsPermissionsTitle = ({
  subscriptionRootPermissions,
  queryRootPermissions,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  isSubscriptionStreamingEnabled,
}: Props) => (
  <div className="flex items-center">
    <div className="mr-2">Root fields permissions</div>
    <OverlayTrigger
      placement="right"
      overlay={
        <Tooltip id="tooltip-root-fields-permissions">
          Choose root fields to be added under the query and subscription root
          fields.
        </Tooltip>
      }
    >
      <FaQuestionCircle aria-hidden="true" />
    </OverlayTrigger>
    <i className="ml-md font-normal">
      {getSectionStatusLabel({
        subscriptionRootPermissions,
        queryRootPermissions,
        hasEnabledAggregations,
        hasSelectedPrimaryKeys,
        isSubscriptionStreamingEnabled,
      })}
    </i>
  </div>
);
