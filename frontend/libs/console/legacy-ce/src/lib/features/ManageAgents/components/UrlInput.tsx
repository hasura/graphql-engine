import { useFormContext } from 'react-hook-form';
import { FormValues } from './AddAgentForm';
import { InputField, Radio } from '../../../new-components/Form';

export const UrlInput = () => {
  const { watch } = useFormContext<FormValues>();
  const selectedType = watch('url.type');
  console.log({ selectedType });
  return (
    <div>
      <Radio
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
      {selectedType === 'url' ? (
        <InputField
          label="URL"
          name="url.value"
          type="text"
          tooltip="The URL of the data connector agent"
          placeholder="Enter the URI of the agent"
        />
      ) : (
        <InputField
          label="Environment Variable"
          name="url.value"
          type="text"
          tooltip="The Environment variable that contains the URL of the data connector agent"
          placeholder="DC_AGENT_URL_ENV_VAR"
        />
      )}
    </div>
  );
};
