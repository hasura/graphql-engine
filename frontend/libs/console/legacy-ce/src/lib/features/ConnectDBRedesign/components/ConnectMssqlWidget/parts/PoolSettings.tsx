import { InputField } from '@hasura/shared/ui';

export const PoolSettings = ({ name }: { name: string }) => {
  return (
    <>
      <InputField
        name={`${name}.totalMaxConnections`}
        label="Total Max Connections"
        tooltip="Maximum number of database connections"
        fieldProps={{
          type: 'number',
          placeholder: '1000',
        }}
      />
      <InputField
        name={`${name}.idleTimeout`}
        label="Idle Timeout"
        tooltip="The idle timeout (in seconds) per connection"
        fieldProps={{
          type: 'number',
          placeholder: '5',
        }}
      />
    </>
  );
};
