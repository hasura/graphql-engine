import { NumberInputField } from '../../ConnectPostgresWidget/parts/NumberInput';

export const PoolSettings = ({ name }: { name: string }) => {
  return (
    <>
      <NumberInputField
        name={`${name}.totalMaxConnections`}
        label="Total Max Connections"
        placeholder="1000"
        tooltip="Maximum number of database connections"
      />
      <NumberInputField
        name={`${name}.idleTimeout`}
        label="Idle Timeout"
        placeholder="180"
        tooltip="The idle timeout (in seconds) per connection"
      />
    </>
  );
};
