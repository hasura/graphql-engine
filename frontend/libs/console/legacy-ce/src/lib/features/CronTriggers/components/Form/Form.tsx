import React, { useState } from 'react';
import { SimpleForm, InputField } from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import { Analytics } from '../../../Analytics';
import { useReadOnlyMode } from '../../../../hooks';
import { getConfirmation } from '../../../../components/Common/utils/jsUtils';
import { useFormContext } from 'react-hook-form';
import { RequestTransform } from '../../../../metadata/types';
import { schema, Schema } from './schema';
import {
  CronPayloadInput,
  AdvancedSettings,
  CronScheduleSelector,
} from './components';
import {
  getCronTriggerCreateQuery,
  getCronTriggerDeleteQuery,
  getCronTriggerUpdateQuery,
} from './utils';
import { useCronMetadataMigration, useDefaultValues } from './hooks';
import { CronRequestTransformation } from './components/CronRequestTransformation';
import { FaShieldAlt } from 'react-icons/fa';

type Props = {
  /**
   * Cron trigger name if the form is modifying an already created cron trigger
   */
  cronTriggerName?: string;
  /**
   * Success callback which can be used to apply custom logic on success, for ex. closing the form
   */
  onSuccess?: (name?: string) => void;
  onDeleteSuccess?: () => void;
};

interface FormContentProps {
  initialTransform?: RequestTransform;
  setTransform: (data: RequestTransform) => void;
}

const FormContent = (props: FormContentProps) => {
  const { setTransform, initialTransform } = props;
  const { watch } = useFormContext();
  const webhookUrl = watch('webhook');
  const payload = watch('payload');
  return (
    <>
      <div className="mb-xs w-1/2">
        <InputField
          name="name"
          label="Name"
          placeholder="Name..."
          tooltip="Give this cron trigger a friendly name"
        />
      </div>
      <div className="mb-xs w-1/2">
        <InputField
          name="comment"
          label="Comment / Description"
          placeholder="Comment / Description..."
          tooltip="A statement to help describe the cron trigger in brief"
        />
      </div>
      <hr className="my-md" />
      <div className="mb-xs w-1/2">
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
      <div className="mb-xs w-1/2">
        <CronScheduleSelector />
      </div>
      <div className="mb-xs w-1/2">
        <CronPayloadInput />
      </div>
      <hr className="my-md" />

      <div className="my-md w-1/2">
        <AdvancedSettings />
      </div>
      <hr className="my-md" />
      <div className="mb-xs pt-xs w-1/2">
        <CronRequestTransformation
          webhookUrl={webhookUrl}
          payload={payload}
          initialValue={initialTransform}
          onChange={setTransform}
        />
      </div>
    </>
  );
};

const CronTriggersForm = (props: Props) => {
  const { onSuccess, onDeleteSuccess, cronTriggerName } = props;
  const {
    requestTransform,
    data: defaultValues,
    isLoading,
    isError,
  } = useDefaultValues({
    cronTriggerName,
  });

  const { data: readOnlyMode } = useReadOnlyMode();
  const [transform, setTransform] = useState<RequestTransform | undefined>();

  const { mutation: deleteMutation } = useCronMetadataMigration({
    onSuccess: onDeleteSuccess,
    successMessage: 'Cron trigger deleted successfully',
    errorMessage: 'Something went wrong while deleting cron trigger',
  });

  const { mutation: updateMutation } = useCronMetadataMigration({
    successMessage: 'Cron trigger updated successfully',
    errorMessage: 'Something went wrong while updating cron trigger',
  });

  const { mutation: createMutation } = useCronMetadataMigration({
    successMessage: 'Cron trigger created successfully',
    errorMessage: 'Something went wrong while creating cron trigger',
  });

  const onDelete = () => {
    const isOk = getConfirmation(
      'Are you sure you want to delete this cron trigger?',
      true,
      cronTriggerName
    );
    if (isOk && cronTriggerName) {
      deleteMutation.mutate({
        query: getCronTriggerDeleteQuery(cronTriggerName),
      });
    }
  };

  const onSubmit = (values: Record<string, unknown>) => {
    if (cronTriggerName) {
      const isOk =
        cronTriggerName === values?.name ||
        getConfirmation(
          'Renaming a trigger deletes the current trigger and creates a new trigger with this configuration. All the events of the current trigger will be dropped. Are you sure you want to continue?',
          true,
          'RENAME'
        );
      if (isOk) {
        updateMutation.mutate(
          {
            query: getCronTriggerUpdateQuery(
              cronTriggerName,
              values as Schema,
              transform
            ),
          },
          {
            onSuccess: () => {
              onSuccess?.(values?.name as string);
            },
          }
        );
      }
    } else {
      createMutation.mutate(
        {
          query: getCronTriggerCreateQuery(values as Schema, transform),
        },
        {
          onSuccess: () => {
            onSuccess?.(values?.name as string);
          },
        }
      );
    }
  };

  if (isLoading) {
    return <div>Loading cron trigger data...</div>;
  }

  if (isError) {
    return <div>Something went wrong while loading cron trigger data</div>;
  }

  return (
    <SimpleForm
      schema={schema}
      onSubmit={onSubmit}
      options={{ defaultValues }}
      className="overflow-y-hidden p-4"
    >
      <FormContent
        initialTransform={requestTransform}
        setTransform={setTransform}
      />
      <div className="flex items-center mb-lg gap-2">
        {cronTriggerName ? (
          <Analytics
            name="events-tab-button-create-cron-trigger"
            passHtmlAttributesToChildren
          >
            <Button
              type="submit"
              mode="primary"
              isLoading={updateMutation.isLoading}
              disabled={readOnlyMode}
            >
              Update Cron Trigger
            </Button>
          </Analytics>
        ) : (
          <Analytics
            name="events-tab-button-update-cron-trigger"
            passHtmlAttributesToChildren
          >
            <Button
              type="submit"
              mode="primary"
              isLoading={createMutation.isLoading}
              disabled={readOnlyMode}
            >
              Add Cron Trigger
            </Button>
          </Analytics>
        )}

        {cronTriggerName && (
          <Analytics
            name="events-tab-button-delete-cron-trigger"
            passHtmlAttributesToChildren
          >
            <Button
              onClick={onDelete}
              mode="destructive"
              isLoading={deleteMutation.isLoading}
              disabled={readOnlyMode}
            >
              Delete trigger
            </Button>
          </Analytics>
        )}
      </div>
    </SimpleForm>
  );
};

export default CronTriggersForm;
