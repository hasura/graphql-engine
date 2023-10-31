import { useState } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaPlay } from 'react-icons/fa';
import z from 'zod';

import { Dialog } from '../../../../../../new-components/Dialog';

import { CodeEditorField } from '../../../../../../new-components/Form';
import { KeyValueListSelector } from '../../../../../../new-components/KeyValuePairsSelector';
import { schema } from './DynamicDBRouting';
import { FailureCard } from './FailureCard';
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
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const { connection_template, ...dataToSave } = { ...data };
  localStorage.setItem(
    'dynamic-db-routing-context',
    JSON.stringify(dataToSave)
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
            ?.reduce((acc, { key, value }) => {
              acc[key] = value;
              return acc;
            }, {} as Record<string, string>),
          session: values?.session_variables
            ?.filter(({ checked }) => checked)
            ?.reduce((acc, { key, value }) => {
              acc[key] = value;
              return acc;
            }, {} as Record<string, string>),
          query: {
            operation_type: values?.operation_type || 'query',
            ...(values?.operation_name
              ? { operation_name: values.operation_name }
              : {}),
          },
        },
      },
      {
        onSuccess: data => {
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
        onError: error => {
          setSuccess(undefined);
          setFailure({ message: error.message });
        },
      }
    );
  };

  return (
    <Dialog
      size="xxxl"
      hasBackdrop
      title="Validate Dynamic Routing"
      description="Validate Dynamic Routing to make sure it meets your need"
      onClose={onClose}
    >
      <>
        <div className="flex gap-4">
          <div className="flex-1">
            <div className="px-6 mb-4">
              <div className="mb-4 font-semibold text-muted">Headers</div>
              <KeyValueListSelector name="validation.headers" />
            </div>
            <div className="px-6 mb-4">
              <div className="mb-4 font-semibold text-muted">
                Session Variables
              </div>

              <KeyValueListSelector name="validation.session_variables" />
            </div>
            <div className="px-6 mb-4">
              <div className="mb-4 font-semibold text-muted">
                Operation Type and Name
              </div>
              <OperationField />
            </div>
          </div>
          <div className="flex-1 px-6 mb-4">
            <div className="mb-4 font-semibold text-muted">Template</div>
            <CodeEditorField
              noErrorPlaceholder
              name="validation.connection_template"
              editorOptions={editorOptions}
            />
          </div>
        </div>
        {failure && <FailureCard message={failure.message} />}

        {success && (
          <SuccessCard routingTo={success.routing_to} value={success.value} />
        )}

        <Dialog.Footer
          callToDeny="Close"
          callToAction="Validate"
          isLoading={isLoading}
          onClose={onClose}
          callToActionType="button"
          onSubmit={validateTemplate}
          callToActionIcon={<FaPlay className="w-3 h-3" />}
          onSubmitAnalyticsName="data-tab-dynamic-db-routing-validate-connection-submit"
          onCancelAnalyticsName="data-tab-dynamic-db-routing-validate-connection-cancel"
        />
      </>
    </Dialog>
  );
};
