import { InputField } from '../../../../../new-components/Form';

const commonFieldProps: Partial<React.InputHTMLAttributes<HTMLInputElement>> = {
  onWheelCapture: e => e.currentTarget.blur(),
};

export const PoolSettings = ({ name }: { name: string }) => {
  return (
    <>
      <InputField
        type="number"
        name={`${name}.totalMaxConnections`}
        label="Total Max Connections"
        placeholder="1000"
        tooltip="Maximum number of database connections"
        fieldProps={commonFieldProps}
      />
      <InputField
        type="number"
        name={`${name}.idleTimeout`}
        label="Idle Timeout"
        placeholder="180"
        tooltip="The idle timeout (in seconds) per connection"
        fieldProps={commonFieldProps}
      />
      <InputField
        type="number"
        name={`${name}.retries`}
        label="Retries"
        placeholder="1"
        tooltip="Number of retries to perform"
        fieldProps={commonFieldProps}
      />
      <InputField
        type="number"
        name={`${name}.poolTimeout`}
        label="Pool Timeout"
        placeholder="360"
        tooltip="Maximum time (in seconds) to wait while acquiring a Postgres connection from the pool"
        fieldProps={commonFieldProps}
      />
      <InputField
        type="number"
        name={`${name}.connectionLifetime`}
        label="Connection Lifetime"
        placeholder="600"
        tooltip="Time (in seconds) from connection creation after which the connection should be destroyed and a new one created. A value of 0 indicates we should never destroy an active connection. If 0 is passed, memory from large query results may not be reclaimed."
        fieldProps={commonFieldProps}
      />
    </>
  );
};
