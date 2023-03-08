import { Select } from '../../../new-components/Form';
import React from 'react';
import { useFormContext } from 'react-hook-form';
import { useAvailableDrivers } from '../hooks';
import { useSyncBackToReduxState } from '../hooks/useSyncBackToReduxState';

interface DriverProps {
  onDriverChange: (driver: string, name: string) => void;
}

export const Driver = (props: DriverProps) => {
  const { data: availableDrivers } = useAvailableDrivers();

  const { watch } = useFormContext<{ driver: string; name: string }>();

  const { onDriverChange } = props;

  const driver = watch('driver');
  const name = watch('name');

  useSyncBackToReduxState(onDriverChange, name, driver);

  if (!availableDrivers) return null;

  const options = availableDrivers.map(d => ({
    value: d.name,
    label: `${d.displayName} ${d.release === 'GA' ? '' : `(${d.release})`}`,
  }));

  return (
    <div>
      <Select options={options} name="driver" label="Data Source Driver" />
    </div>
  );
};
