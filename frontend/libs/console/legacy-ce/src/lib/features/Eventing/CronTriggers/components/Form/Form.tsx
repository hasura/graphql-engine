import { useState } from 'react';
import {
  SimpleForm,
  InputField,
  Button,
  Separator,
  hasuraToast,
  useDestructivePrompt,
  useHasuraAlert,
} from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { useFormContext } from 'react-hook-form';
import { schema, Schema } from './schema';
import {
  CronPayloadInput,
  AdvancedSettings,
  CronScheduleSelector,
} from './components';
import { useDefaultValues } from './hooks';
import { CronRequestTransformation } from './components/CronRequestTransformation';
import { FaShieldAlt } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';
import { EventRequestTransform } from '@hasura/shared/types';
import type { CronTrigger } from '@hasura/shared/types';
import {
  useCreateCronTrigger,
  useDeleteCronTrigger,
  useUpdateCronTrigger,
} from '@hasura/metadata/api';
import { transformFormData } from './utils';

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
  const destructivePrompt = useDestructivePrompt();
  const { hasuraPrompt } = useHasuraAlert();
  const [transform, setTransform] = useState<
    EventRequestTransform | undefined
  >();

  const { mutateAsync: deleteMutation, isPending: isDeleting } =
    useDeleteCronTrigger();

  const { mutateAsync: updateMutation, isPending: isUpdating } =
    useUpdateCronTrigger();

  const { mutateAsync: createMutation, isPending: isCreating } =
    useCreateCronTrigger();

  const onDelete = () => {
    if (!currentTrigger?.name) {
      return;
    }

    const triggerName = currentTrigger.name;

    destructivePrompt({
      resourceName: triggerName,
      resourceType: 'cron trigger',
      destroyTerm: 'delete',
      appendTerm: 'This also deletes the associated metadata.',
      onConfirm: () =>
        deleteMutation(triggerName)
          .then(() => {
            onDeleteSuccess?.();
            return true;
          })
          .catch(() => false),
    });
  };

  const onSubmit = (values: Record<string, unknown>) => {
    const runUpdate = async () => {
      if (!currentTrigger) {
        return false;
      }

      return updateMutation({
        name: currentTrigger.name,
        args: transformFormData(values as Schema, false, transform),
      })
        .then(() => {
          onSuccess?.(values?.name as string);
          return true;
        })
        .catch(() => false);
    };

    if (currentTrigger) {
      if (currentTrigger.name === values?.name) {
        runUpdate();
        return;
      }

      // Renaming a cron trigger drops the current trigger (and all of its
      // scheduled events) and recreates it with the new configuration, so keep
      // the original typed-"RENAME" confirmation as a hard safeguard.
      hasuraPrompt({
        title: 'Rename cron trigger',
        message:
          'Renaming a trigger deletes the current trigger and creates a new trigger with this configuration. All the events of the current trigger will be dropped. Are you sure you want to continue?',
        destructive: true,
        confirmText: 'Continue',
        promptLabel: 'Type RENAME to confirm this action.',
        onCloseAsync: async (result) => {
          if (!result.confirmed) {
            return { withSuccess: false };
          }

          if (result.promptValue !== 'RENAME') {
            hasuraToast({
              type: 'error',
              title: 'Entry Not Matching',
              message: `Your entry "${result.promptValue}" does not match "RENAME".`,
            });
            return { withSuccess: false };
          }

          return runUpdate().then((withSuccess) => ({ withSuccess }));
        },
      });
    } else {
      return createMutation(
        transformFormData(values as Schema, false, transform),
      )
        .then(() => {
          onSuccess?.(values?.name as string);
          return true;
        })
        .catch(() => false);
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
              loading={isUpdating}
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
              loading={isCreating}
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
              disabled={readOnlyMode || isDeleting}
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
