import type { Source, Table } from '@hasura/shared/types';
import {
  TableFkRelationships,
  useAlterForeignKey,
  ViolationAction,
} from '@hasura/metadata/data-source';
import { fkToFormValues, FkFormValues, ForeignKeyForm } from './ForeignKeyForm';

export const ForeignKeyEditForm = ({
  source,
  table,
  foreignKey,
  onDone,
}: {
  source: Source;
  table: Table;
  foreignKey: TableFkRelationships;
  onDone: () => void;
}) => {
  const {
    mutate: alterForeignKey,
    isPending,
    error,
  } = useAlterForeignKey({
    onSuccess: () => onDone(),
  });

  const onSubmit = (values: FkFormValues) => {
    const parsedRef = values.referenceTable as Table;
    if (!parsedRef) return;
    alterForeignKey({
      source: { name: source.name, kind: source.kind },
      constraintName: foreignKey.name ?? '',
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
      title={`Edit ${foreignKey.name ?? 'Foreign Key'}`}
      defaultValues={fkToFormValues(foreignKey, source)}
      submitLabel="Save changes"
      isPending={isPending}
      onSubmit={onSubmit}
      onClose={onDone}
      error={error}
    />
  );
};

export default ForeignKeyEditForm;
