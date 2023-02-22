import { Collapsible } from '../../../../../new-components/Collapsible';
import { InputField, Radio, Select } from '../../../../../new-components/Form';
import { isProConsole } from '../../../../../utils';
import { useFormContext } from 'react-hook-form';
import { ConnectionInfoSchema } from '../schema';
import { BooleanInput } from './BooleanInput';
import { PoolSettings } from './PoolSettings';
import { SslSettings } from './SslSettings';

export const ConnectionInfo = ({
  name,
  hideOptions,
}: {
  name: string;
  hideOptions: string[];
}) => {
  const options = [
    { value: 'databaseUrl', label: 'Database URL' },
    { value: 'envVar', label: 'Enviromnent variable' },
    { value: 'connectionParams', label: 'Connection Parameters' },
  ].filter(option => !hideOptions.includes(option.value));

  const { watch } = useFormContext<Record<string, ConnectionInfoSchema>>();

  const connectionType = watch(`${name}.databaseUrl.connectionType`);

  return (
    <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
      <div className="bg-white py-1.5 font-semibold">
        <Radio
          name={`${name}.databaseUrl.connectionType`}
          label="Connect Database via"
          options={options}
          orientation="horizontal"
          tooltip="Enviroment variable recomennded"
        />
      </div>

      {connectionType === 'databaseUrl' ? (
        <InputField
          name={`${name}.databaseUrl.url`}
          label="Database URL"
          placeholder="postgresql://username:password@hostname:port/database"
        />
      ) : connectionType === 'envVar' ? (
        <InputField
          name={`${name}.databaseUrl.envVar`}
          label="Environment variable"
          placeholder="HASURA_GRAPHQL_DB_URL_FROM_ENV"
        />
      ) : (
        <>
          <InputField
            name={`${name}.databaseUrl.username`}
            label="Username"
            placeholder="postgres_user"
          />
          <InputField
            name={`${name}.databaseUrl.password`}
            label="Password"
            type="password"
            placeholder="5432"
          />
          <InputField
            name={`${name}.databaseUrl.database`}
            label="Database name"
            placeholder="postgres"
          />
          <InputField
            name={`${name}.databaseUrl.host`}
            label="Host"
            placeholder="localhost"
          />
          <InputField
            name={`${name}.databaseUrl.port`}
            label="Port"
            type="number"
            placeholder="port"
          />
        </>
      )}
      <Collapsible
        triggerChildren={
          <span className="font-semibold text-muted">Advanced Settings</span>
        }
      >
        <PoolSettings name={`${name}.poolSettings`} />
        <Select
          options={[
            {
              value: 'read-committed',
              label: 'read-committed',
            },
            {
              value: 'repeatable-read',
              label: 'repeatable-read',
            },
            {
              value: 'serializable',
              label: 'serializable',
            },
          ]}
          name={`${name}.isolationLevel`}
          label="Isolation Level"
          tooltip="The transaction isolation level in which the queries made to the source will be run"
        />
        <BooleanInput
          name={`${name}.usePreparedStatements`}
          label="Use Prepared Statements"
          tooltip="Prepared statements are disabled by default"
        />
        {isProConsole(window.__env) && (
          <Collapsible
            triggerChildren={
              <div className="font-semibold text-muted">
                SSL Certificates Settings
                <span className="px-1.5 italic font-light">
                  (Certificates will be loaded from{' '}
                  <a href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database">
                    environment variables
                  </a>
                  )
                </span>
              </div>
            }
          >
            <SslSettings name={`${name}.sslSettings`} />
          </Collapsible>
        )}
      </Collapsible>
    </div>
  );
};
