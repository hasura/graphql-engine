import { z } from 'zod';
import { Analytics } from '../../../../../../features/Analytics';
import { Button } from '../../../../../../new-components/Button';
import { Dialog } from '../../../../../../new-components/Dialog';
import {
  GraphQLSanitizedInputField,
  useConsoleForm,
} from '../../../../../../new-components/Form';
import { SanitizeTips } from '../../../../../../utils/sanitizeGraphQLFieldNames';
import { implement } from '../../../../../../utils/zodUtils';

export type CustomFunctionFieldsFormValues = {
  custom_name: string;
  function: string;
  function_aggregate: string;
};

export type CustomFunctionFieldsFormProps = {
  onSubmit: (data: CustomFunctionFieldsFormValues) => void;
  onClose: () => void;
  callToAction?: string;
  callToActionLoadingText?: string;
  callToDeny?: string;
  isLoading: boolean;
  defaultValues: CustomFunctionFieldsFormValues;
};

const schema = implement<CustomFunctionFieldsFormValues>().with({
  custom_name: z.string(),
  function: z.string(),
  function_aggregate: z.string(),
});

export const CustomFunctionFieldsForm = ({
  isLoading,
  onClose,
  callToAction = 'Save',
  callToActionLoadingText = 'Saving...',
  callToDeny = 'Cancel',
  onSubmit,
  defaultValues,
}: CustomFunctionFieldsFormProps) => {
  const { methods, Form } = useConsoleForm({
    schema,
    options: {
      defaultValues,
    },
  });

  const { watch, handleSubmit } = methods;

  const values = watch();

  const onSubmitHandler = (data: CustomFunctionFieldsFormValues) => {
    handleSubmit(onSubmit)();
  };

  const hasValues = Object.values(values).some(value => !!value);

  const reset = () => {
    methods.reset({
      custom_name: '',
      function: '',
      function_aggregate: '',
    });
  };

  return (
    <Form onSubmit={onSubmitHandler}>
      <div>
        <div className="px-4 pb-sm">
          <SanitizeTips />
          <div className="mb-4 flex justify-end">
            <Button disabled={!hasValues} size="sm" onClick={reset}>
              Clear All Fields
            </Button>
          </div>

          {/* custom_name */}
          <div>
            <Analytics name="custom_name" htmlAttributesToRedact="value">
              <GraphQLSanitizedInputField
                hideTips
                clearButton
                name="custom_name"
                label="Custom Function Name"
                placeholder="custom_name"
              />
            </Analytics>
          </div>

          {/* function */}
          <div>
            <Analytics name="function" htmlAttributesToRedact="value">
              <GraphQLSanitizedInputField
                hideTips
                clearButton
                name="function"
                label="Function name root field"
                placeholder="function_name"
              />
            </Analytics>
          </div>

          {/* function_aggregate */}
          <div>
            <Analytics name="function_aggregate" htmlAttributesToRedact="value">
              <GraphQLSanitizedInputField
                hideTips
                clearButton
                name="function_aggregate"
                label="Function aggregate root field"
                placeholder="function_aggregate"
              />
            </Analytics>
          </div>
        </div>
        <Dialog.Footer
          callToAction={callToAction}
          isLoading={isLoading}
          callToActionLoadingText={callToActionLoadingText}
          callToDeny={callToDeny}
          onClose={onClose}
          className="sticky w-full bottom-0 left-0"
        />
      </div>
    </Form>
  );
};
