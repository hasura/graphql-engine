import { Controller } from 'react-hook-form';
import { z } from 'zod';
import { Dialog } from '../../../../../new-components/Dialog';
import {
  FieldWrapper,
  GraphQLSanitizedInputField,
  InputField,
  Select,
  useConsoleForm,
} from '../../../../../new-components/Form';
import { Switch } from '../../../../../new-components/Switch';
import { implement } from '../../../../../utils/zodUtils';

import { FormDebug } from '../../../../../new-components/Form/dev-components/FormDebug';
import { NativeQueryArgumentNormalized } from '../types';

/**
 *
 * Not currently using this in favor of an editable table approach
 * However, if design/product decide they prefer a dialog approach, this may be needed again
 *
 */
export const AddParameterDialog = ({
  onCancel,
  onAdd,
}: {
  onCancel: () => void;
  onAdd: (argument: NativeQueryArgumentNormalized) => void;
}) => {
  const { Form, methods } = useConsoleForm({
    schema: implement<NativeQueryArgumentNormalized>().with({
      name: z.string().min(1),
      type: z.string().min(1),
      default_value: z.string().optional(),
      required: z.boolean().optional(),
    }),
    options: {},
  });

  return (
    <Form
      onSubmit={values => {
        onAdd(values);
      }}
    >
      <Dialog
        hasBackdrop
        title={'Add Query Parameter'}
        footer={{
          callToAction: 'Add',
          callToDeny: 'Cancel',
          onClose: () => {
            onCancel();
          },
        }}
      >
        <div className="p-4">
          <FormDebug />
          <div className="flex flex-col">
            <GraphQLSanitizedInputField
              hideTips
              label="Parameter Name"
              name="name"
            />
            <Select
              name="type"
              label="Type"
              options={[
                { value: 'string', label: 'string' },
                { value: 'int', label: 'int' },
              ]}
            />
            <InputField label="Default Value" name="default_value" />
            <Controller
              name="required"
              control={methods.control}
              render={({ field, fieldState, formState }) => (
                <FieldWrapper label="Required" error={fieldState.error}>
                  <Switch
                    checked={field.value}
                    onCheckedChange={field.onChange}
                  />
                </FieldWrapper>
              )}
            />
          </div>
        </div>
      </Dialog>
    </Form>
  );
};
