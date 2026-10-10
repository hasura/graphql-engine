import { useState } from 'react';
import { SimpleForm, InputField, Button, Separator } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { getConfirmation } from '@hasura/shared/utils';
import { useFormContext } from 'react-hook-form';
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
import { Flex } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';
import { EventRequestTransform } from '@hasura/shared/types';
import type { CronTrigger } from '@hasura/shared/types';

type Props = {
  /**
   * Cron trigger if the form is modifying an already created cron trigger
   */
  currentTrigger?: CronTrigger;
  /**
   * Success callback which can be used to apply custom logic on success, for ex. closing the form
   */
  onSuccess?: (name?: string) => void;
  onDeleteSuccess?: () => void;
};

interface FormContentProps {
  initialTransform?: EventRequestTransform;
  setTransform: (data: EventRequestTransform) => void;
}

const FormContent = ({ setTransform, initialTransform }: FormContentProps) => {
  const { watch } = useFormContext();
  const webhookUrl = watch('webhook');
  const payload = watch('payload');

  return (
    <Flex direction="column" className="w-8/12" gap="4">
      <InputField
        name="name"
        label="Name"
        fieldProps={{ placeholder: 'Name...' }}
        tooltip="Give this cron trigger a friendly name"
      />
      <InputField
        name="comment"
        label="Comment / Description"
        fieldProps={{ placeholder: 'Comment / Description...' }}
        tooltip="A statement to help describe the cron trigger in brief"
      />
      <Separator size="4" />
      <InputField
        learnMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
        tooltipIcon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
        name="webhook"
        label="Webhook URL"
        fieldProps={{
          placeholder: 'http://httpbin.org/post or {{MY_WEBHOOK_URL}}/handler',
        }}
        tooltip="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
        description="Note: Provide an URL or use an env var to template the handler URL if you have different URLs for multiple environments."
      />

      <CronScheduleSelector />
      <CronPayloadInput />
      <Separator size="4" />
      <AdvancedSettings />
      <Separator size="4" />
      <CronRequestTransformation
        webhookUrl={webhookUrl}
        payload={payload}
        initialValue={initialTransform}
        onChange={setTransform}
      />
    </Flex>
  );
};

const CronTriggersForm = (props: Props) => {
  const { onSuccess, onDeleteSuccess, currentTrigger } = props;
  const { requestTransform, data: defaultValues } = useDefaultValues({
    currentTrigger,
  });

  const { readOnlyMode } = useAppContext();
  const [transform, setTransform] = useState<
    EventRequestTransform | undefined
  >();

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
    if (!currentTrigger) {
      return;
    }

    const isOk = getConfirmation(
      'Are you sure you want to delete this cron trigger?',
      true,
      currentTrigger.name,
    );
    if (isOk && currentTrigger?.name) {
      deleteMutation.mutate({
        query: getCronTriggerDeleteQuery(currentTrigger.name),
      });
    }
  };

  const onSubmit = (values: Record<string, unknown>) => {
    if (currentTrigger) {
      const isOk =
        currentTrigger.name === values?.name ||
        getConfirmation(
          'Renaming a trigger deletes the current trigger and creates a new trigger with this configuration. All the events of the current trigger will be dropped. Are you sure you want to continue?',
          true,
          'RENAME',
        );
      if (isOk) {
        updateMutation.mutate(
          {
            query: getCronTriggerUpdateQuery(
              currentTrigger.name,
              values as Schema,
              transform,
            ),
          },
          {
            onSuccess: () => {
              onSuccess?.(values?.name as string);
            },
          },
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
        },
      );
    }
  };

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
      <Flex align="center" gap="2" className="mt-4">
        {currentTrigger ? (
          <Analytics
            name="events-tab-button-create-cron-trigger"
            passHtmlAttributesToChildren
          >
            <Button
              type="submit"
              mode="primary"
              loading={updateMutation.isPending}
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
              loading={createMutation.isPending}
              disabled={readOnlyMode}
            >
              Add Cron Trigger
            </Button>
          </Analytics>
        )}

        {currentTrigger && (
          <Analytics
            name="events-tab-button-delete-cron-trigger"
            passHtmlAttributesToChildren
          >
            <Button
              onClick={onDelete}
              mode="destructive"
              loading={deleteMutation.isPending}
              disabled={readOnlyMode}
            >
              Delete trigger
            </Button>
          </Analytics>
        )}
      </Flex>
    </SimpleForm>
  );
};

export default CronTriggersForm;
