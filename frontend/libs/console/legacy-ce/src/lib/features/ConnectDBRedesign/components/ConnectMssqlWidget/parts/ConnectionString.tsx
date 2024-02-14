import { InputField, Radio } from '../../../../../new-components/Form';
import { useFormContext } from 'react-hook-form';
import { ConnectionInfoSchema } from '../schema';
import { WarningCard } from '../../Common/WarningCard';

export const ConnectionString = ({ name }: { name: string }) => {
  const options = [
    { value: 'databaseUrl', label: 'Database URL' },
    { value: 'envVar', label: 'Environment variable' },
  ];

  const { watch } = useFormContext<Record<string, ConnectionInfoSchema>>();

  const connectionType = watch(`${name}.connectionType`);

  return (
    <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
      <div className="bg-white py-1.5 font-semibold">
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
            label="Database URL"
            placeholder="Driver={ODBC Driver 18 for SQL Server};Server=serveraddress;Database=dbname;Uid=username;Pwd=password"
          />
        </>
      ) : (
        <InputField
          name={`${name}.envVar`}
          label="Environment variable"
          placeholder="HASURA_GRAPHQL_DB_URL_FROM_ENV"
        />
      )}
    </div>
  );
};
