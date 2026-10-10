import { useFormContext } from 'react-hook-form';
import { FormValues } from './AddAgentForm';
import { InputField, RadioGroupField } from '@hasura/shared/ui';

export const UrlInput = () => {
  const { watch } = useFormContext<FormValues>();
  const selectedType = watch('url.type');

  return (
    <div>
      <div>
        <RadioGroupField
          label="URL"
          name="url.type"
          options={[
            { label: 'Using URL value', value: 'url' },
            {
              label: 'Using Environment Variable (recommended)',
              value: 'envVar',
            },
          ]}
          orientation="horizontal"
        />
      </div>
      {selectedType === 'url' ? (
        <InputField
          label="URL"
          name="url.value"
          tooltip="The URL of the data connector agent"
          fieldProps={{
            type: 'text',
            placeholder: 'Enter the URI of the agent',
          }}
        />
      ) : (
        <InputField
          label="Environment Variable"
          name="url.value"
          tooltip="The Environment variable that contains the URL of the data connector agent"
          fieldProps={{
            type: 'text',
            placeholder: 'DC_AGENT_URL_ENV_VAR',
          }}
        />
      )}
    </div>
  );
};
