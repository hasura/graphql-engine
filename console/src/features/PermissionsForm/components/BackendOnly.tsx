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
          <input type="checkbox" {...register('backendOnly')} />

          <p>Allow from backends only</p>
        </label>
      </Collapse.Content>
    </Collapse>
  );
};

export default BackendOnlySection;
