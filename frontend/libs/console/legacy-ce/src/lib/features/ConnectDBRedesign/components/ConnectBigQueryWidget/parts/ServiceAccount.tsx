import {
  CodeEditorField,
  InputField,
  Radio,
} from '../../../../../new-components/Form';
import { useFormContext } from 'react-hook-form';
import { BigQueryConnectionSchema } from '../schema';
import { WarningCard } from '../../Common/WarningCard';

export const ServiceAccount = ({ name }: { name: string }) => {
  const options = [
    { value: 'serviceAccountKey', label: 'Service Account Key' },
    { value: 'envVar', label: 'Environment variable' },
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
          tooltip="Environment variable recommended"
        />
      </div>

      {connectionType === 'serviceAccountKey' ? (
        <>
          <WarningCard />
          <CodeEditorField name={`${name}.value`} label="Service Account" />
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
