import { NumberInputField } from '../../ConnectPostgresWidget/parts/NumberInput';

export const Timeout = ({ name }: { name: string }) => {
  return (
    <NumberInputField
      name={name}
      label="Timeout (in seconds)"
      placeholder="In Seconds"
    />
  );
};
