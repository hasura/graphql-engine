import React, { useState } from 'react';
import { useFormContext } from 'react-hook-form';
import { Collapse } from '../../../../new-components/deprecated';

import { isFeatureSupported } from '../../../../dataSources';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { QueryType } from '../../types';
import { PermissionsConfirmationModal } from './RootFieldPermissions/PermissionsConfirmationModal';
import {
  getPermissionsModalTitle,
  getPermissionsModalDescription,
} from './RootFieldPermissions/PermissionsConfirmationModal.utils';
import { isPermissionModalDisabled } from '../utils/getPermissionModalStatus';

export interface AggregationProps {
  queryType: QueryType;
  roleName: string;
  defaultOpen?: boolean;
}

export const AggregationSection: React.FC<AggregationProps> = ({
  queryType,
  roleName,
  defaultOpen,
}) => {
  const { watch, setValue } = useFormContext();
  const [showConfirmationModal, setShowConfirmationModal] = useState(false);
  // if no row permissions are selected selection should be disabled
  const disabled = useIsDisabled(queryType);

  const [enabled, queryRootFields, subscriptionRootFields] = watch([
    'aggregationEnabled',
    'query_root_fields',
    'subscription_root_fields',
  ]);

  if (!isFeatureSupported('tables.permissions.aggregation')) {
    return null;
  }

  const handleUpdate = () => {
    setValue('aggregationEnabled', !enabled);
    setValue(
      'query_root_fields',
      (queryRootFields ?? []).filter(
        (field: string) => field !== 'select_aggregate'
      )
    );
    setValue(
      'subscription_root_fields',
      (subscriptionRootFields ?? []).filter(
        (field: string) => field !== 'select_aggregate'
      )
    );
  };

  const permissionsModalTitle = getPermissionsModalTitle({
    scenario: 'aggregate',
    role: roleName,
  });

  const permissionsModalDescription =
    getPermissionsModalDescription('aggregate');

  return (
    <>
      <Collapse
        title="Aggregation queries permissions"
        tooltip="Allow queries with aggregate functions like sum, count, avg,
    max, min, etc"
        status={enabled ? 'Enabled' : 'Disabled'}
        data-test="toggle-agg-permission"
        disabled={disabled}
        defaultOpen={defaultOpen || enabled}
      >
        <Collapse.Content>
          <div title={disabled ? 'Set row permissions first' : ''}>
            <label className="flex items-center gap-4">
              <input
                type="checkbox"
                title={disabled ? 'Set row permissions first' : ''}
                disabled={disabled}
                className="m-0 mt-0 rounded shadow-sm border border-gray-300 hover:border-gray-400 focus:ring-yellow-400"
                checked={enabled}
                onChange={() => {
                  const pkRootFieldsAreSelected =
                    queryRootFields?.includes('select_aggregate') ||
                    subscriptionRootFields?.includes('select_aggregate');
                  const hideModal = isPermissionModalDisabled();
                  if (
                    !showConfirmationModal &&
                    pkRootFieldsAreSelected &&
                    !hideModal
                  ) {
                    setShowConfirmationModal(true);
                    return;
                  }
                  handleUpdate();
                }}
              />
              <span>
                Allow role <strong>{roleName}</strong> to make aggregation
                queries
              </span>
            </label>
          </div>
        </Collapse.Content>
      </Collapse>
      {showConfirmationModal && (
        <PermissionsConfirmationModal
          title={permissionsModalTitle}
          description={permissionsModalDescription}
          onClose={() => setShowConfirmationModal(false)}
          onSubmit={() => {
            handleUpdate();
            setShowConfirmationModal(false);
          }}
        />
      )}
    </>
  );
};

export default AggregationSection;
