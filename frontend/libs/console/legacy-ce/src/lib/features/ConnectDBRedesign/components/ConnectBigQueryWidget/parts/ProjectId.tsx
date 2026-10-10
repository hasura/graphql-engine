import { Card, InputField, RadioGroupField } from '@hasura/shared/ui';
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
    <Card size="2">
      <div>
        <RadioGroupField
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
            fieldProps={{
              placeholder: 'Project ID',
            }}
          />
        </>
      ) : (
        <InputField
          name={`${name}.envVar`}
          label="Environment variable"
          fieldProps={{
            placeholder: 'HASURA_GRAPHQL_DB_URL_FROM_ENV',
          }}
        />
      )}
    </Card>
  );
};
