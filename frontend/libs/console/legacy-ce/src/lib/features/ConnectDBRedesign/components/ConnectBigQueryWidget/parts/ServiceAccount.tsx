import {
  Card,
  CodeEditorField,
  InputField,
  RadioGroupField,
} from '@hasura/shared/ui';
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
    <Card size="2">
      <div>
        <RadioGroupField
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
          <CodeEditorField
            name={`${name}.value`}
            label="Service Account"
            editorProps={{
              mode: 'json',
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
