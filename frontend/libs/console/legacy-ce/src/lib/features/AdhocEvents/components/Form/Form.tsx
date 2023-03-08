import React from 'react';
import { InputField, SimpleForm } from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import { schema, Schema } from './schema';
import { useDefaultValues } from './hooks';
import { useOneOffScheduledTriggerMigration } from './hooks/useOneOffScheduledTriggerMigration';
import { getScheduledEventCreateQuery } from './utils';
import {
  AdvancedSettings,
  RetryConfiguration,
  ScheduleEventPayloadInput,
} from './components';
import { ScheduledTime } from './components/ScheduledTime';
import { FaShieldAlt } from 'react-icons/fa';

type Props = {
  /**
   * Success callback which can be used to apply custom logic on success, for ex. closing the form
   */
  onSuccess?: () => void;
};

const OneOffScheduledEventForm = (props: Props) => {
  const { onSuccess } = props;
  const { data: defaultValues } = useDefaultValues();
  const { mutation } = useOneOffScheduledTriggerMigration({ onSuccess });

  const onSubmit = (values: Record<string, unknown>) => {
    mutation.mutate({
      query: getScheduledEventCreateQuery(values as Schema),
    });
  };

  return (
    <SimpleForm
      schema={schema}
      onSubmit={onSubmit}
      options={{ defaultValues }}
      className="overflow-y-hidden p-4"
    >
      <>
        <div className="mb-md">
          <InputField
            name="comment"
            label="Comment / Description"
            placeholder="Comment / Description..."
            tooltip="A statement to help describe the scheduled event in brief"
          />
        </div>
        <div className="mb-md chromatic-ignore">
          <ScheduledTime />
        </div>
        <div className="mb-md">
          <InputField
            learnMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
            tooltipIcon={
              <FaShieldAlt className="h-4 text-muted cursor-pointer" />
            }
            name="webhook"
            label="Webhook URL"
            placeholder="http://httpbin.org/post or {{MY_WEBHOOK_URL}}/handler"
            tooltip="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
            description="Note: Provide an URL or use an env var to template the handler URL if you have different URLs for multiple environments."
          />
        </div>
        <div className="mb-md">
          <ScheduleEventPayloadInput />
        </div>
        <div className="mb-md">
          <AdvancedSettings />
        </div>
        <div className="mb-md">
          <RetryConfiguration />
        </div>
        <div className="flex items-center mb-lg">
          <Button
            data-testid="create-scheduled-event"
            type="submit"
            mode="primary"
            isLoading={mutation.isLoading}
          >
            Create scheduled event
          </Button>
        </div>
      </>
    </SimpleForm>
  );
};

export default OneOffScheduledEventForm;
