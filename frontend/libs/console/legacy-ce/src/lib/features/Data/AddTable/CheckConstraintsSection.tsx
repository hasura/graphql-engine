import { useState } from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { Flex } from '@radix-ui/themes';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
import { Button, Text } from '@hasura/shared/ui';
import { Section } from '../ModifyTable/parts';
import {
  CheckConstraintForm,
  CheckConstraintFormValues,
  emptyCheckConstraintFormValues,
} from '../ModifyTable/components/CheckConstraintForm';
import { AddTableFormValues } from './schema';

/** `null` = add a new constraint, a number = edit the one at that index. */
type DialogState = { index: number | null } | undefined;

export const CheckConstraintsSection = () => {
  const { control } = useFormContext<AddTableFormValues>();
  const checkConstraints = useFieldArray({ control, name: 'checkConstraints' });
  const [dialog, setDialog] = useState<DialogState>();

  const editingConstraint =
    dialog?.index != null ? checkConstraints.fields[dialog.index] : undefined;

  const closeDialog = () => setDialog(undefined);

  const onSubmit = (values: CheckConstraintFormValues) => {
    if (dialog?.index != null) {
      checkConstraints.update(dialog.index, values);
    } else {
      checkConstraints.append(values);
    }
    closeDialog();
  };

  return (
    <Section
      headerText="Check Constraints"
      tooltipMessage="A check constraint allows you to specify if the value in a certain column must satisfy a specific condition."
    >
      {checkConstraints.fields.map((field, index) => (
        <Flex key={field.id} align="center" gap="2" className="mb-1">
          <Button
            type="button"
            size="sm"
            leftIcon={FaEdit}
            aria-label={`Edit check constraint ${index + 1}`}
            onClick={() => setDialog({ index })}
          >
            Edit
          </Button>
          <Button
            type="button"
            mode="destructive"
            size="sm"
            leftIcon={FaTrash}
            aria-label={`Remove check constraint ${index + 1}`}
            onClick={() => checkConstraints.remove(index)}
          >
            Remove
          </Button>
          <Text weight="medium">{field.name}</Text>
          <Text className="font-mono" color="gray">
            CHECK ({field.check})
          </Text>
        </Flex>
      ))}
      <div className="mt-2">
        <Button
          type="button"
          mode="default"
          size="1"
          leftIcon={FaPlus}
          onClick={() => setDialog({ index: null })}
        >
          Add Check Constraint
        </Button>
      </div>

      {dialog && (
        <CheckConstraintForm
          title={
            editingConstraint ? 'Edit Check Constraint' : 'Add Check Constraint'
          }
          submitLabel={editingConstraint ? 'Save' : 'Add'}
          defaultValues={
            editingConstraint
              ? { name: editingConstraint.name, check: editingConstraint.check }
              : emptyCheckConstraintFormValues
          }
          onSubmit={onSubmit}
          onClose={closeDialog}
        />
      )}
    </Section>
  );
};

export default CheckConstraintsSection;
