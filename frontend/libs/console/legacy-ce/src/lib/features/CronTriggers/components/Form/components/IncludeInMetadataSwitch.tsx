import React from 'react';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { useFormContext } from 'react-hook-form';
import { Switch } from '../../../../../new-components/Switch';

export const IncludeInMetadataSwitch = () => {
  const { watch, setValue } = useFormContext();
  const include_in_metadata = watch('include_in_metadata');

  const setIncludeInMetadataStatus = (value: boolean) => {
    setValue('include_in_metadata', value);
  };
  return (
    <>
      <label className="block flex items-center text-gray-600 font-semibold mb-xs">
        Include in Metadata
        <IconTooltip message="If enabled, this cron trigger will be included in the metadata of GraphqL Engine i.e. it will be a part of the metadata that is exported as migrations" />
      </label>
      <div className="relative w-full max-w-xl mb-xs">
        <Switch
          checked={include_in_metadata}
          onCheckedChange={setIncludeInMetadataStatus}
        />
      </div>
    </>
  );
};
