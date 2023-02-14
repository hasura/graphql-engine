import { InputField, Radio } from '@/new-components/Form';
import { useFormContext } from 'react-hook-form';
import { ConnectionInfoSchema } from '../schema';

export const ConnectionInfo = ({ name }: { name: string }) => {
  const options = [
    { value: 'databaseUrl', label: 'Database URL' },
    { value: 'envVar', label: 'Enviromnent variable' },
  ];

  const { watch } = useFormContext<Record<string, ConnectionInfoSchema>>();

  const connectionType = watch(`${name}.connectionString.connectionType`);

  return (
    <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
      <div className="bg-white py-1.5 font-semibold">
        <Radio
          name={`${name}.connectionString.connectionType`}
          label="Connect Database via"
          options={options}
          orientation="horizontal"
          tooltip="Enviroment variable recomennded"
        />
      </div>

      {connectionType === 'databaseUrl' ? (
        <InputField
          name={`${name}.connectionString.url`}
          label="Database URL"
          placeholder="Driver={ODBC Driver 18 for SQL Server};Server=serveraddress;Database=dbname;Uid=username;Pwd=password"
        />
      ) : (
        <InputField
          name={`${name}.connectionString.envVar`}
          label="Environment variable"
          placeholder="HASURA_GRAPHQL_DB_URL_FROM_ENV"
        />
      )}
    </div>
  );
};
