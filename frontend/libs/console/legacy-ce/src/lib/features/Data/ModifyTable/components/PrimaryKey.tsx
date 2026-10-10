import { useState } from 'react';
import { Flex, Strong } from '@radix-ui/themes';
import {
  Button,
  IconButton,
  SkeletonList,
  Text,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
import {
  getDatabaseMethods,
  useAlterPrimaryKey,
  useCreatePrimaryKey,
  useDropPrimaryKey,
  useTableColumns,
  useTablePrimaryKey,
} from '@hasura/metadata/data-source';
import { ModifyTableProps } from '../types';
import { PrimaryKeyForm, PrimaryKeyFormValues } from './PrimaryKeyForm';

/**
 * Primary key editor: shows the table's PK with add / edit / remove actions,
 * each opening a dialog or confirm and gated on the real driver method
 * (`createPrimaryKey` / `alterPrimaryKey` / `dropPrimaryKey`). PG family only.
 */
export const PrimaryKey = ({ source, table }: ModifyTableProps) => {
  const dbMethods = getDatabaseMethods(source.kind);
  const canCreate = Boolean(dbMethods.modify?.createPrimaryKey);
  const canAlter = Boolean(dbMethods.modify?.alterPrimaryKey);
  const canDrop = Boolean(dbMethods.modify?.dropPrimaryKey);
  const canRead = Boolean(dbMethods.introspection.getPrimaryKey);

  const [isDialogOpen, setIsDialogOpen] = useState(false);
  const closeDialog = () => setIsDialogOpen(false);

  const destructiveConfirm = useDestructiveConfirm();

  const { data: existingPk, isLoading } = useTablePrimaryKey(
    { source, table: table.table },
    { enabled: canRead },
  );

  const { data: columnData } = useTableColumns({ source, table: table.table });
  const columnOptions = (columnData?.columns ?? []).map((c) => ({
    value: c.name,
    label: c.name,
  }));

  const {
    mutate: createPrimaryKey,
    isPending: creating,
    error: createError,
  } = useCreatePrimaryKey({ onSuccess: closeDialog });
  const {
    mutate: alterPrimaryKey,
    isPending: altering,
    error: alterError,
  } = useAlterPrimaryKey({ onSuccess: closeDialog });
  const { mutateAsync: dropPrimaryKey } = useDropPrimaryKey();

  const onSubmit = (values: PrimaryKeyFormValues) => {
    if (existingPk) {
      alterPrimaryKey({
        source: { name: source.name, kind: source.kind },
        table: table.table,
        constraintName: existingPk.constraintName,
        columns: values.columns,
        previousColumns: existingPk.columns,
      });
    } else {
      const tbl = table.table as { name?: string };
      createPrimaryKey({
        source: { name: source.name, kind: source.kind },
        table: table.table,
        constraintName: `${tbl.name}_pkey`,
        columns: values.columns,
      });
    }
  };

  if (canRead && isLoading) return <SkeletonList count={1} />;

  // Nothing actionable for this driver.
  if (!canCreate && !canAlter) return null;

  return (
    <>
      {existingPk ? (
        <Flex align="center" gap="2" className="mb-1">
          {canAlter && (
            <IconButton
              type="button"
              size="1"
              mode="default"
              icon={FaEdit}
              title={`Edit primary key ${existingPk.constraintName}`}
              onClick={() => setIsDialogOpen(true)}
            />
          )}
          {canDrop && (
            <IconButton
              type="button"
              mode="destructive"
              variant="outline"
              size="1"
              icon={FaTrash}
              title={`Remove primary key ${existingPk.constraintName}`}
              onClick={() =>
                destructiveConfirm({
                  resourceName: existingPk.constraintName,
                  resourceType: 'Primary Key',
                  onConfirm: async () => {
                    try {
                      await dropPrimaryKey({
                        source: { name: source.name, kind: source.kind },
                        table: table.table,
                        constraintName: existingPk.constraintName,
                        columns: existingPk.columns,
                      });
                      return true;
                    } catch {
                      return false;
                    }
                  },
                })
              }
            />
          )}
          <Text>
            <Strong>{existingPk.constraintName}</Strong> &middot;{' '}
            {existingPk.columns.join(', ')}
          </Text>
        </Flex>
      ) : (
        <>
          <Text as="p">This table has no primary key.</Text>
          {canCreate && (
            <div className="mt-2">
              <Button
                type="button"
                size="sm"
                mode="default"
                leftIcon={FaPlus}
                onClick={() => setIsDialogOpen(true)}
              >
                Add Primary Key
              </Button>
            </div>
          )}
        </>
      )}

      {isDialogOpen && (
        <PrimaryKeyForm
          title={
            existingPk ? `Edit ${existingPk.constraintName}` : 'Add Primary Key'
          }
          submitLabel={existingPk ? 'Save changes' : 'Add Primary Key'}
          defaultValues={{ columns: existingPk?.columns ?? [] }}
          columnOptions={columnOptions}
          isPending={creating || altering}
          error={existingPk ? alterError : createError}
          onSubmit={onSubmit}
          onClose={closeDialog}
        />
      )}
    </>
  );
};

export default PrimaryKey;
