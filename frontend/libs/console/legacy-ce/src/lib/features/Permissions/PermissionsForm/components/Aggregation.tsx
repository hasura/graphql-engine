import React, { useState } from 'react';
import { useFormContext } from 'react-hook-form';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { PermissionsConfirmationModal } from './RootFieldPermissions/PermissionsConfirmationModal';
import {
  getPermissionsModalTitle,
  getPermissionsModalDescription,
} from './RootFieldPermissions/PermissionsConfirmationModal.utils';
import { isPermissionModalDisabled } from '../utils/getPermissionModalStatus';
import { DataQueryType, Source } from '@hasura/shared/types';
import { Checkbox, Collapsible, CollapsibleHeader } from '@hasura/shared/ui';
import { getDatabaseMethods } from '@hasura/metadata/data-source';
import { PermissionsSchema } from '../../schema';

export interface AggregationProps {
  dataSource: Source;
  queryType: DataQueryType;
  roleName: string;
  defaultOpen?: boolean;
}

export const AggregationSection: React.FC<AggregationProps> = ({
  dataSource,
  queryType,
  roleName,
  defaultOpen,
}) => {
  const { watch, setValue } = useFormContext<PermissionsSchema>();
  const [showConfirmationModal, setShowConfirmationModal] = useState(false);
  // if no row permissions are selected selection should be disabled
  const disabled = useIsDisabled(queryType);

  const [enabled, queryRootFields, subscriptionRootFields] = watch([
    'aggregationEnabled',
    'query_root_fields',
    'subscription_root_fields',
  ]);

  if (
    !getDatabaseMethods(dataSource.kind).check.isFeatureSupported(
      'tables.permissions.aggregation',
    )
  ) {
    return null;
  }

  const handleUpdate = () => {
    setValue('aggregationEnabled', !enabled);
    if (queryRootFields?.length) {
      setValue(
        'query_root_fields',
        queryRootFields.filter((field: string) => field !== 'select_aggregate'),
      );
    }
    if (subscriptionRootFields?.length) {
      setValue(
        'subscription_root_fields',
        subscriptionRootFields.filter(
          (field: string) => field !== 'select_aggregate',
        ),
      );
    }
  };

  const permissionsModalTitle = getPermissionsModalTitle({
    scenario: 'aggregate',
    role: roleName,
  });

  const permissionsModalDescription =
    getPermissionsModalDescription('aggregate');

  return (
    <>
      <Collapsible
        data-test="toggle-agg-permission"
        disabled={disabled}
        defaultOpen={defaultOpen || enabled}
        triggerChildren={
          <CollapsibleHeader
            title="Aggregation queries permissions"
            tooltip="Allow queries with aggregate functions like sum, count, avg, max, min, etc"
            status={enabled ? 'Enabled' : 'Disabled'}
          />
        }
      >
        <div title={disabled ? 'Set row permissions first' : ''}>
          <label className="flex items-center gap-4">
            <Checkbox
              title={disabled ? 'Set row permissions first' : ''}
              disabled={disabled}
              value={enabled}
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
            >
              Allow role <strong>{roleName}</strong> to make aggregation queries
            </Checkbox>
          </label>
        </div>
      </Collapsible>
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
