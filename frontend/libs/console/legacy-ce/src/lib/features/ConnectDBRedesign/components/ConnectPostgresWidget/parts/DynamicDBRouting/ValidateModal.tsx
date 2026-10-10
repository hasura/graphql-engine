import { useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { useFormContext } from 'react-hook-form';
import { FaPlay } from 'react-icons/fa';
import z from 'zod';
import {
  Dialog,
  CodeEditorField,
  KeyValueListSelector,
  DialogFooter,
  FieldLabel,
  IndicatorCard,
} from '@hasura/shared/ui';
import { schema } from './DynamicDBRouting';
import { useDynamicDbRouting } from './hooks/useDynamicDbRouting';
import { OperationField } from './OperationField';
import { SuccessCard } from './SuccessCard';

const editorOptions = {
  minLines: 18,
  maxLines: 18,
  showLineNumbers: true,
  useSoftTabs: true,
  showPrintMargin: false,
  showGutter: true,
  wrap: true,
};

const saveFormData = (data: z.infer<typeof schema>['validation']) => {
  const { connection_template, ...dataToSave } = { ...data };
  localStorage.setItem(
    'dynamic-db-routing-context',
    JSON.stringify(dataToSave),
  );
};

interface ValidateModalProps {
  sourceName: string;
  onClose: () => void;
}

export const ValidateModal = (props: ValidateModalProps) => {
  const { onClose, sourceName } = props;

  const [success, setSuccess] = useState<{
    routing_to: string;
    value?: string;
  }>();
  const [failure, setFailure] = useState<{ message: string }>();

  const { getValues } = useFormContext<z.infer<typeof schema>>();

  const { testConnectionTemplate, isLoading } = useDynamicDbRouting({
    sourceName,
  });

  const validateTemplate = () => {
    const { validation: values } = getValues();
    saveFormData(values);
    testConnectionTemplate(
      {
        connection_template: {
          template: values?.connection_template || '',
        },
        source_name: sourceName,
        request_context: {
          headers: values?.headers
            ?.filter(({ checked }) => checked)
            ?.reduce(
              (acc, { key, value }) => {
                acc[key] = value;
                return acc;
              },
              {} as Record<string, string>,
            ),
          session: values?.session_variables
            ?.filter(({ checked }) => checked)
            ?.reduce(
              (acc, { key, value }) => {
                acc[key] = value;
                return acc;
              },
              {} as Record<string, string>,
            ),
          query: {
            operation_type: values?.operation_type || 'query',
            ...(values?.operation_name
              ? { operation_name: values.operation_name }
              : {}),
          },
        },
      },
      {
        onSuccess: (data) => {
          setFailure(undefined);
          setSuccess({
            routing_to: (
              data.result as unknown as {
                routing_to: string;
                value: string;
              }
            ).routing_to,
            value: (
              data.result as unknown as {
                routing_to: string;
                value: string;
              }
            ).value,
          });
        },
        onError: (error) => {
          setSuccess(undefined);
          setFailure({ message: error.message });
        },
      },
    );
  };

  return (
    <Dialog
      size="xxxl"
      title="Validate Dynamic Routing"
      description="Validate Dynamic Routing to make sure it meets your need"
      onClose={onClose}
    >
      <>
        <Flex gap="4">
          <div className="flex-1">
            <div className="mb-4">
              <FieldLabel className="mb-4" label="Headers" />
              <KeyValueListSelector name="validation.headers" />
            </div>
            <div className="mb-4">
              <FieldLabel className="mb-4" label="Session Variables" />
              <KeyValueListSelector name="validation.session_variables" />
            </div>
            <div className="mb-4">
              <FieldLabel className="mb-4" label="Operation Type and Name" />
              <OperationField />
            </div>
          </div>
          <div className="flex-1 mb-4">
            <CodeEditorField
              noErrorPlaceholder
              label="Template"
              name="validation.connection_template"
              editorOptions={editorOptions}
              editorProps={{
                mode: 'json',
              }}
            />
          </div>
        </Flex>
        {failure && (
          <div className="mt-4">
            <IndicatorCard
              status="negative"
              showIcon
              title="Your request failed:"
            >
              {failure.message}
            </IndicatorCard>
          </div>
        )}

        {success && (
          <SuccessCard routingTo={success.routing_to} value={success.value} />
        )}

        <DialogFooter
          callToDeny="Close"
          callToAction="Validate"
          isLoading={isLoading}
          onClose={onClose}
          callToActionProps={{
            type: 'button',
            leftIcon: FaPlay,
          }}
          onSubmit={validateTemplate}
          onSubmitAnalyticsName="data-tab-dynamic-db-routing-validate-connection-submit"
          onCancelAnalyticsName="data-tab-dynamic-db-routing-validate-connection-cancel"
        />
      </>
    </Dialog>
  );
};
