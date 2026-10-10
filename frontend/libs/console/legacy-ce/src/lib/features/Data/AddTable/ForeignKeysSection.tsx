import { useState } from 'react';
import { useFieldArray, useFormContext, useWatch } from 'react-hook-form';
import { Flex } from '@radix-ui/themes';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
import { Button, Text } from '@hasura/shared/ui';
import type { Source } from '@hasura/shared/types';
import { Section } from '../ModifyTable/parts';
import {
  emptyFkFormValues,
  FkFormValues,
  ForeignKeyForm,
} from '../ModifyTable/components/ForeignKeyForm';
import { AddTableFormValues } from './schema';
import { getTableDisplayName } from '@hasura/shared/utils';

type ForeignKeyValues = AddTableFormValues['foreignKeys'][number];

const foreignKeyLabel = (fk: ForeignKeyValues): string => {
  const ref = fk.referenceTable;
  const refLabel = ref ? getTableDisplayName(ref) : '?';
  const from = fk.columnMappings.map((m) => m.from).join(', ');
  const to = fk.columnMappings.map((m) => m.to).join(', ');
  return `(${from}) → ${refLabel} (${to})`;
};

const toFormValues = (fk: ForeignKeyValues, source: Source): FkFormValues => {
  const defaults = emptyFkFormValues(source);
  return {
    referenceTable: fk.referenceTable,
    columnMappings: fk.columnMappings,
    onUpdate: fk.onUpdate ?? defaults.onUpdate,
    onDelete: fk.onDelete ?? defaults.onDelete,
  };
};

/** `null` = add a new foreign key, a number = edit the one at that index. */
type DialogState = { index: number | null } | undefined;

export const ForeignKeysSection = ({ source }: { source: Source }) => {
  const { control } = useFormContext<AddTableFormValues>();
  const foreignKeys = useFieldArray({ control, name: 'foreignKeys' });
  const [dialog, setDialog] = useState<DialogState>();

  const columns = useWatch<AddTableFormValues>({ name: 'columns' }) as
    AddTableFormValues['columns'] | undefined;

  const ownColumnOptions = (columns ?? [])
    .filter((c) => c.name.trim() !== '')
    .map((c) => ({ value: c.name, label: c.name }));

  const editingForeignKey =
    dialog?.index != null ? foreignKeys.fields[dialog.index] : undefined;

  const closeDialog = () => setDialog(undefined);

  const onSubmit = (values: FkFormValues) => {
    if (dialog?.index != null) {
      foreignKeys.update(dialog.index, values);
    } else {
      foreignKeys.append(values);
    }
    closeDialog();
  };

  return (
    <Section
      headerText="Foreign Keys"
      tooltipMessage="Optional. Link columns of this table to another table's columns. Foreign keys can also be added later from Modify."
    >
      {foreignKeys.fields.map((field, index) => (
        <Flex key={field.id} align="center" gap="2" className="mb-1">
          <Button
            type="button"
            size="sm"
            leftIcon={FaEdit}
            aria-label={`Edit foreign key ${index + 1}`}
            onClick={() => setDialog({ index })}
          >
            Edit
          </Button>
          <Button
            type="button"
            mode="destructive"
            size="sm"
            leftIcon={FaTrash}
            aria-label={`Remove foreign key ${index + 1}`}
            onClick={() => foreignKeys.remove(index)}
          >
            Remove
          </Button>
          <Text weight="medium">{foreignKeyLabel(field)}</Text>
          <Text className="italic" color="gray">
            on update {field.onUpdate} · on delete {field.onDelete}
          </Text>
        </Flex>
      ))}
      <div className="mt-2">
        <Button
          type="button"
          className="mt-1"
          mode="default"
          size="1"
          leftIcon={FaPlus}
          onClick={() => setDialog({ index: null })}
        >
          Add Foreign Key
        </Button>
      </div>

      {dialog && (
        <ForeignKeyForm
          source={source}
          ownColumnOptions={ownColumnOptions}
          title={editingForeignKey ? 'Edit Foreign Key' : 'Add Foreign Key'}
          submitLabel={editingForeignKey ? 'Save' : 'Add'}
          defaultValues={
            editingForeignKey
              ? toFormValues(editingForeignKey, source)
              : emptyFkFormValues(source)
          }
          onSubmit={onSubmit}
          onClose={closeDialog}
        />
      )}
    </Section>
  );
};

export default ForeignKeysSection;
