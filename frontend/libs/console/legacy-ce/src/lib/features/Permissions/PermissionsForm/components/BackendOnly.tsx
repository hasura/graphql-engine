import React from 'react';
import { useFormContext } from 'react-hook-form';
import { DataQueryType } from '@hasura/shared/types';
import { Collapsible, CollapsibleHeader, Switch } from '@hasura/shared/ui';

export interface BackEndOnlySectionProps {
  queryType: DataQueryType;
  defaultOpen?: boolean;
}

export const BackendOnlySection: React.FC<BackEndOnlySectionProps> = ({
  queryType,
  defaultOpen,
}) => {
  const { setValue, watch } = useFormContext();

  const enabled = watch('backendOnly');

  return (
    <Collapsible
      defaultOpen={defaultOpen || enabled}
      triggerChildren={
        <CollapsibleHeader
          title="Backend only"
          tooltip={`When enabled, this ${queryType} mutation is accessible only via
              "trusted backends"`}
          data-test="toogle-backend-only"
          status={enabled ? 'Enabled' : 'Not enabled'}
        />
      }
    >
      <label className="flex items-center gap-4">
        <Switch
          value={enabled}
          onChange={(switched) => setValue('backendOnly', switched)}
        />
        <span>Allow from backends only</span>
      </label>
    </Collapsible>
  );
};

export default BackendOnlySection;
