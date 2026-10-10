import { z } from 'zod';
import {
  Dialog,
  GraphQLSanitizedInputField,
  InputField,
  useConsoleForm,
  FormDebug,
  SelectField,
  SwitchField,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
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
  const { Form } = useConsoleForm({
    schema: z.object({
      name: z.string().min(1),
      type: z.string().min(1),
      default_value: z.string().optional(),
      required: z.boolean().optional(),
    }),
    options: {},
  });

  return (
    <Form
      onSubmit={(values) => {
        onAdd(values);
      }}
    >
      <Dialog
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
          <Flex direction="column">
            <GraphQLSanitizedInputField
              hideTips
              label="Parameter Name"
              name="name"
            />
            <SelectField
              name="type"
              label="Type"
              options={[
                { value: 'string', label: 'string' },
                { value: 'int', label: 'int' },
              ]}
            />
            <InputField label="Default Value" name="default_value" />
            <SwitchField name="required">Required</SwitchField>
          </Flex>
        </div>
      </Dialog>
    </Form>
  );
};
