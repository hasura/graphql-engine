import React from 'react';
import { InputField, SimpleForm } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
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
            name="webhook"
            label="Webhook URL"
            placeholder="https://httpbin.com/post"
            tooltip="The HTTP URL that should be triggered."
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
