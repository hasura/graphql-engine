import { InputField, Radio } from '../../../../../new-components/Form';
import { useFormContext } from 'react-hook-form';
import { BigQueryConnectionSchema } from '../schema';
import { WarningCard } from '../../Common/WarningCard';

export const ProjectId = ({ name }: { name: string }) => {
  const options = [
    { value: 'value', label: 'Project ID value' },
    { value: 'envVar', label: 'Environment variable' },
  ];

  const { watch } =
    useFormContext<
      Record<string, BigQueryConnectionSchema['configuration']['projectId']>
    >();

  const connectionType = watch(`${name}.type`);

  return (
    <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
      <div className="bg-white py-1.5 font-semibold">
        <Radio
          name={`${name}.type`}
          label="Project ID"
          options={options}
          orientation="horizontal"
          tooltip="Environment variable recommended"
        />
      </div>

      {connectionType === 'value' ? (
        <>
          <WarningCard />
          <InputField
            name={`${name}.value`}
            label="Project ID"
            placeholder="Project ID"
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
