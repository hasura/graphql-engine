import React from 'react';
import { useFormContext } from 'react-hook-form';

import { Collapse } from '../../../../new-components/deprecated';
import { QueryType } from '../../types';
import { Switch } from '../../../../new-components/Switch/Switch';

export interface BackEndOnlySectionProps {
  queryType: QueryType;
  defaultOpen?: boolean;
}

export const BackendOnlySection: React.FC<BackEndOnlySectionProps> = ({
  queryType,
  defaultOpen,
}) => {
  const { setValue, watch } = useFormContext();

  const enabled = watch('backendOnly');

  return (
    <Collapse defaultOpen={defaultOpen || enabled}>
      <Collapse.Header
        title="Backend only"
        tooltip={`When enabled, this ${queryType} mutation is accessible only via
              "trusted backends"`}
        data-test="toogle-backend-only"
        status={enabled ? 'Enabled' : 'Not enabled'}
        // learnMoreRef="https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#backend-only"
      />
      <Collapse.Content>
        <label className="flex items-center gap-4">
          <Switch
            checked={enabled}
            onCheckedChange={switched => setValue('backendOnly', switched)}
          />
          <span>Allow from backends only</span>
        </label>
      </Collapse.Content>
    </Collapse>
  );
};

export default BackendOnlySection;
