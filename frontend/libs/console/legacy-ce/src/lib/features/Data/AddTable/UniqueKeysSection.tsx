import { useState } from 'react';
import { useFieldArray, useFormContext, useWatch } from 'react-hook-form';
import { Flex } from '@radix-ui/themes';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
import { Button, Text } from '@hasura/shared/ui';
import { Section } from '../ModifyTable/parts';
import {
  emptyUniqueKeyFormValues,
  UniqueKeyForm,
  UniqueKeyFormValues,
} from '../ModifyTable/components/UniqueKeyForm';
import { AddTableFormValues } from './schema';

/** `null` = add a new unique key, a number = edit the one at that index. */
type DialogState = { index: number | null } | undefined;

/**
 * Composite unique-key editor. Each key is a set of column names; the payload
 * mapping resolves them to column indices and de-duplicates against the
 * per-column `unique` flags.
 */
export const UniqueKeysSection = () => {
  const { control } = useFormContext<AddTableFormValues>();
  const uniqueKeys = useFieldArray({ control, name: 'uniqueKeys' });
  const [dialog, setDialog] = useState<DialogState>();

  const columns = useWatch<AddTableFormValues>({ name: 'columns' }) as
    AddTableFormValues['columns'] | undefined;

  const columnOptions = (columns ?? [])
    .filter((c) => c.name.trim() !== '')
    .map((c) => ({ value: c.name, label: c.name }));

  const editingKey =
    dialog?.index != null ? uniqueKeys.fields[dialog.index] : undefined;

  const closeDialog = () => setDialog(undefined);

  const onSubmit = ({ columns: keyColumns }: UniqueKeyFormValues) => {
    if (dialog?.index != null) {
      uniqueKeys.update(dialog.index, { columns: keyColumns });
    } else {
      uniqueKeys.append({ columns: keyColumns });
    }
    closeDialog();
  };

  return (
    <Section
      headerText="Unique Keys"
      tooltipMessage="Optional. Add multi-column unique constraints. Single-column uniqueness can also be set via the column's Unique checkbox."
    >
      {uniqueKeys.fields.map((field, index) => (
        <Flex key={field.id} align="center" gap="2" className="mb-1">
          <Button
            type="button"
            size="sm"
            leftIcon={FaEdit}
            aria-label={`Edit unique key ${index + 1}`}
            onClick={() => setDialog({ index })}
          >
            Edit
          </Button>
          <Button
            type="button"
            mode="destructive"
            size="sm"
            leftIcon={FaTrash}
            aria-label={`Remove unique key ${index + 1}`}
            onClick={() => uniqueKeys.remove(index)}
          >
            Remove
          </Button>
          <Text weight="medium">UNIQUE ({field.columns.join(', ')})</Text>
        </Flex>
      ))}
      <div className="mt-2">
        <Button
          type="button"
          leftIcon={FaPlus}
          mode="default"
          size="1"
          onClick={() => setDialog({ index: null })}
        >
          Add Unique Key
        </Button>
      </div>

      {dialog && (
        <UniqueKeyForm
          title={editingKey ? 'Edit Unique Key' : 'Add Unique Key'}
          submitLabel={editingKey ? 'Save' : 'Add'}
          withName={false}
          columnOptions={columnOptions}
          defaultValues={
            editingKey
              ? { name: '', columns: editingKey.columns }
              : emptyUniqueKeyFormValues
          }
          onSubmit={onSubmit}
          onClose={closeDialog}
        />
      )}
    </Section>
  );
};

export default UniqueKeysSection;
