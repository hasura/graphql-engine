import React from 'react';
import { useFormContext } from 'react-hook-form';

import { Collapse } from '@/new-components/deprecated';

import { isFeatureSupported } from '@/dataSources';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { QueryType } from '../types';

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
  const { register, watch } = useFormContext();

  // if no row permissions are selected selection should be disabled
  const disabled = useIsDisabled(queryType);

  const enabled = watch('aggregationEnabled');

  if (!isFeatureSupported('tables.permissions.aggregation')) {
    return null;
  }

  return (
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
              {...register('aggregationEnabled')}
            />
            <span>
              Allow role <strong>{roleName}</strong> to make aggregation queries
            </span>
          </label>
        </div>
      </Collapse.Content>
    </Collapse>
  );
};

export default AggregationSection;
