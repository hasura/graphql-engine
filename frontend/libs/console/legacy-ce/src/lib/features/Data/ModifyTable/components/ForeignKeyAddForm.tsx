import type { Source, Table } from '@hasura/shared/types';
import {
  useCreateForeignKey,
  ViolationAction,
} from '@hasura/metadata/data-source';
import {
  emptyFkFormValues,
  FkFormValues,
  ForeignKeyForm,
} from './ForeignKeyForm';

export const ForeignKeyAddForm = ({
  source,
  table,
  onClose,
}: {
  source: Source;
  table: Table;
  onClose: () => void;
}) => {
  const {
    mutate: createForeignKey,
    isPending,
    error,
  } = useCreateForeignKey({
    onSuccess: () => onClose(),
  });

  const onSubmit = (values: FkFormValues) => {
    const parsedRef = values.referenceTable as Table;
    if (!parsedRef) return;
    createForeignKey({
      source: { name: source.name, kind: source.kind },
      from: { table, columns: values.columnMappings.map((m) => m.from) },
      to: { table: parsedRef, columns: values.columnMappings.map((m) => m.to) },
      onUpdate: values.onUpdate as ViolationAction,
      onDelete: values.onDelete as ViolationAction,
    });
  };

  return (
    <ForeignKeyForm
      source={source}
      table={table}
      title="Add Foreign Key"
      defaultValues={emptyFkFormValues(source)}
      submitLabel="Add Foreign Key"
      isPending={isPending}
      onSubmit={onSubmit}
      onClose={onClose}
      error={error}
    />
  );
};

export default ForeignKeyAddForm;
