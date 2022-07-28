import React from 'react';
import { Form, InputField } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { schema, Schema } from './schema';
import {
  CronScheduleSelector,
  CronPayloadInput,
  IncludeInMetadataSwitch,
  AdvancedSettings,
  RetryConfiguration,
} from './components';
import { getCronTriggerCreateQuery, getCronTriggerUpdateQuery } from './utils';
import { useCronMetadataMigration, useDefaultValues } from './hooks';

type Props = {
  /**
   * Cron trigger name if the form is modifying an already created cron trigger
   */
  cronTriggerName?: string;
  /**
   * Success callback which can be used to apply custom logic on success, for ex. closing the form
   */
  onSuccess?: () => void;
};

const CronTriggersForm = (props: Props) => {
  const { onSuccess, cronTriggerName } = props;
  const { data: defaultValues, isLoading, isError } = useDefaultValues({
    cronTriggerName,
  });
  const { mutation } = useCronMetadataMigration({ onSuccess });

  const onSubmit = (values: Record<string, unknown>) => {
    if (cronTriggerName) {
      mutation.mutate({
        query: getCronTriggerUpdateQuery(values as Schema),
      });
    } else {
      mutation.mutate({
        query: getCronTriggerCreateQuery(values as Schema),
      });
    }
  };

  if (isLoading) {
    return <div>Loading cron trigger data...</div>;
  }

  if (isError) {
    return <div>Something went wrong while loading cron trigger data</div>;
  }

  // TODO: type casting defaultValues as any as we need to fix the <Form /> component to accept nullable fields
  return (
    <Form
      schema={schema}
      onSubmit={onSubmit}
      options={{ defaultValues: defaultValues as any }}
      className="overflow-y-hidden p-4"
    >
      {() => (
        <>
          <div className="mb-md">
            <InputField
              name="name"
              label="Name"
              placeholder="Name..."
              tooltip="Give this cron trigger a friendly name"
            />
          </div>
          <div className="mb-md">
            <InputField
              name="comment"
              label="Comment / Description"
              placeholder="Comment / Description..."
              tooltip="A statement to help describe the cron trigger in brief"
            />
          </div>
          <div className="mb-md">
            <InputField
              name="webhook"
              label="Webhook URL"
              placeholder="https://httpbin.com/post"
              tooltip="The HTTP URL that should be triggered. You can also provide the URL from environment variables, e.g. {{MY_WEBHOOK_URL}}"
            />
          </div>
          <div className="mb-md">
            <CronScheduleSelector />
          </div>
          <div className="mb-md">
            <CronPayloadInput />
          </div>
          <div className="mb-md">
            <IncludeInMetadataSwitch />
          </div>
          <div className="mb-md">
            <AdvancedSettings />
          </div>
          <div className="mb-md">
            <RetryConfiguration />
          </div>
          <div className="flex items-center mb-lg">
            <Button type="submit" mode="primary" isLoading={mutation.isLoading}>
              {cronTriggerName ? 'Update Cron Trigger' : 'Add Cron Trigger'}
            </Button>
          </div>
        </>
      )}
    </Form>
  );
};

export default CronTriggersForm;
