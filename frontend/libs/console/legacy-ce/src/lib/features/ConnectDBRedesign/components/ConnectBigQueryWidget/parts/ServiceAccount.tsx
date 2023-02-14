import { InputField, Radio } from '@/new-components/Form';
import { useFormContext } from 'react-hook-form';
import { BigQueryConnectionSchema } from '../schema';

export const ServiceAccount = ({ name }: { name: string }) => {
  const options = [
    { value: 'serviceAccountKey', label: 'Service Account Key' },
    { value: 'envVar', label: 'Enviromnent variable' },
  ];

  const { watch } =
    useFormContext<
      Record<
        string,
        BigQueryConnectionSchema['configuration']['serviceAccount']
      >
    >();

  const connectionType = watch(`${name}.type`);

  return (
    <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
      <div className="bg-white py-1.5 font-semibold">
        <Radio
          name={`${name}.type`}
          label="Connect Database via"
          options={options}
          orientation="horizontal"
          tooltip="Enviroment variable recomennded"
        />
      </div>

      {connectionType === 'serviceAccountKey' ? (
        <InputField
          name={`${name}.value`}
          label="Database URL"
          placeholder=""
        />
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
