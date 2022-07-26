import React from 'react';
import { useFormContext } from 'react-hook-form';

import { Collapse } from '@/new-components/Collapse';
import { QueryType } from '../types';

export interface BackEndOnlySectionProps {
  queryType: QueryType;
  defaultOpen?: boolean;
}

export const BackendOnlySection: React.FC<BackEndOnlySectionProps> = ({
  queryType,
  defaultOpen,
}) => {
  const { register, watch } = useFormContext();

  const enabled = watch('backendOnly');

  return (
    <Collapse defaultOpen={defaultOpen}>
      <Collapse.Header
        title="Backend only"
        tooltip={`When enabled, this ${queryType} mutation is accessible only via
              "trusted backends"`}
        data-test="toogle-backend-only"
        status={enabled ? 'Enabled' : 'Not enabled'}
        // knowMoreRef="https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#backend-only"
      />
      <Collapse.Content>
        <label className="flex items-center gap-4">
          <input
            type="checkbox"
            className="rounded shadow-sm border border-gray-300 hover:border-gray-400 focus:ring-yellow-400 m-0"
            {...register('backendOnly')}
          />

          <span>Allow from backends only</span>
        </label>
      </Collapse.Content>
    </Collapse>
  );
};

export default BackendOnlySection;
