import { isCloudConsole } from '@hasura/shared/utils';
import { InputField } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';

const commonFieldProps = {
  type: 'number' as const,
  onWheelCapture: (e: React.WheelEvent<HTMLInputElement>) =>
    e.currentTarget.blur(),
};

export const PoolSettings = ({ name }: { name: string }) => {
  const { envVars } = useAppContext();

  const isCloud = isCloudConsole(envVars);

  return (
    <div className="mt-2">
      {isCloud && (
        <InputField
          name={`${name}.totalMaxConnections`}
          label="Total Max Connections"
          tooltip="Maximum number of total connections to be maintained across any number of Hasura Cloud instances (default: 1000). Takes precedence over max_connections in Cloud projects."
          fieldProps={{ ...commonFieldProps, placeholder: '1000' }}
        />
      )}

      <InputField
        name={`${name}.maxConnections`}
        label="Max Connections"
        tooltip="Maximum number of connections to be kept in the pool (default: 50)"
        fieldProps={{ ...commonFieldProps, placeholder: '50' }}
      />
      <InputField
        name={`${name}.idleTimeout`}
        label="Idle Timeout"
        tooltip="The idle timeout (in seconds) per connection"
        fieldProps={{
          ...commonFieldProps,
          placeholder: isCloud ? '30' : '180',
        }}
      />
      <InputField
        name={`${name}.retries`}
        label="Retries"
        tooltip="Number of retries to perform"
        fieldProps={{ ...commonFieldProps, placeholder: '1' }}
      />
      <InputField
        name={`${name}.poolTimeout`}
        label="Pool Timeout"
        tooltip="Maximum time (in seconds) to wait while acquiring a Postgres connection from the pool"
        fieldProps={{ ...commonFieldProps, placeholder: '360' }}
      />
      <InputField
        name={`${name}.connectionLifetime`}
        label="Connection Lifetime"
        tooltip="Time (in seconds) from connection creation after which the connection should be destroyed and a new one created. A value of 0 indicates we should never destroy an active connection. If 0 is passed, memory from large query results may not be reclaimed."
        fieldProps={{ ...commonFieldProps, placeholder: '600' }}
      />
    </div>
  );
};
