import React from 'react';
import { InputField, Radio } from '../../../../../new-components/Form';
import { useFormContext } from 'react-hook-form';
import { ConnectionInfoSchema } from '../schema';
import {
  WarningCard,
  WarningCardMetadataDBNotDynamic,
} from '../../Common/WarningCard';
import globals from '../../../../../Globals';

export const DatabaseUrl = ({
  name,
  hideOptions,
}: {
  name: string;
  hideOptions: string[];
}) => {
  const options = [
    { value: 'databaseUrl', label: 'Database URL' },
    { value: 'envVar', label: 'Environment variable' },
    { value: 'connectionParams', label: 'Connection Parameters' },
    { value: 'dynamicFromFile', label: 'Dynamic URL' },
  ]
    .filter(
      option =>
        // Remove this option on cloud, where it is unsupported:
        !(option.value === 'dynamicFromFile' && globals.consoleType === 'cloud')
    )
    .filter(option => !hideOptions.includes(option.value));

  const { watch } = useFormContext<Record<string, ConnectionInfoSchema>>();

  const connectionType = watch(`${name}.connectionType`);

  return (
    <div>
      <div className="py-1.5 font-semibold">
        <Radio
          name={`${name}.connectionType`}
          label="Connect Database via"
          options={options}
          orientation="horizontal"
          tooltip="Environment variable recommended"
        />
      </div>

      {connectionType === 'databaseUrl' ? (
        <>
          <WarningCard />
          <InputField
            name={`${name}.url`}
            key={`${name}.url`}
            label="Database URL"
            placeholder="postgresql://username:password@hostname:port/postgres"
          />
        </>
      ) : connectionType === 'dynamicFromFile' ? (
        <>
          <WarningCardMetadataDBNotDynamic />
          <InputField
            name={`${name}.dynamicFromFile`}
            key={`${name}.dynamicFromFile`}
            label="Path to file that contains the connection string"
            placeholder="/path/to/file/on/server"
          />
        </>
      ) : connectionType === 'envVar' ? (
        <InputField
          name={`${name}.envVar`}
          key={`${name}.envVar`}
          label="Environment variable"
          placeholder="HASURA_GRAPHQL_DB_URL_FROM_ENV"
        />
      ) : (
        <>
          <WarningCard />
          <InputField
            name={`${name}.username`}
            label="Username"
            placeholder="postgres_user"
          />
          <InputField
            name={`${name}.password`}
            label="Password"
            type="password"
            placeholder="password"
          />
          <InputField
            name={`${name}.database`}
            label="Database name"
            placeholder="postgres"
          />
          <InputField
            name={`${name}.host`}
            label="Host"
            placeholder="localhost"
          />
          <InputField
            name={`${name}.port`}
            label="Port"
            type="number"
            placeholder="5432"
          />
        </>
      )}
    </div>
  );
};
