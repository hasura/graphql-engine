import { SelectField } from '@hasura/shared/ui';
import { useAvailableDrivers } from '@hasura/metadata/data-source';

interface DriverProps {
  onDriverChange: (driver: string, name: string) => void;
}

export const Driver = (props: DriverProps) => {
  const { data: availableDrivers } = useAvailableDrivers();

  if (!availableDrivers) return null;

  const options = availableDrivers.map((d) => ({
    value: d.name,
    label: `${d.displayName} ${d.release === 'GA' ? '' : `(${d.release})`}`,
  }));

  return (
    <div>
      <SelectField options={options} name="driver" label="Data Source Driver" />
    </div>
  );
};
