import { useState } from 'react';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  IconButton,
  SkeletonList,
  Text,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { FaPlus, FaTrash } from 'react-icons/fa';
import {
  getDatabaseMethods,
  useCreateIndex,
  useDropIndex,
  useTableColumns,
  useTableIndexes,
} from '@hasura/metadata/data-source';
import { ModifyTableProps } from '../types';
import { emptyIndexFormValues, IndexForm, IndexFormValues } from './IndexForm';

/**
 * Index editor: list / add / remove, each gated on the real driver method
 * (`getTableIndexes` / `createIndex` / `dropIndex`). PG family only.
 */
export const Indexes = ({ source, table }: ModifyTableProps) => {
  const dbMethods = getDatabaseMethods(source.kind);
  const canList = Boolean(dbMethods.introspection.getTableIndexes);
  const canAdd = Boolean(dbMethods.modify?.createIndex);
  const canDrop = Boolean(dbMethods.modify?.dropIndex);

  const destructiveConfirm = useDestructiveConfirm();
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false);

  const { data: indexes = [], isLoading } = useTableIndexes(
    { source, table: table.table },
    { enabled: canList },
  );

  const { data: columnData } = useTableColumns({ source, table: table.table });
  const columnOptions = (columnData?.columns ?? []).map((c) => ({
    value: c.name,
    label: c.name,
  }));

  const { mutateAsync: dropIndex } = useDropIndex();

  const { mutate: createIndex, isPending } = useCreateIndex({
    onSuccess: () => setIsAddDialogOpen(false),
  });

  const onSubmit = (values: IndexFormValues) => {
    createIndex({
      source: { name: source.name, kind: source.kind },
      table: table.table,
      indexName: values.name,
      indexType: values.type,
      columns: values.columns,
      unique: values.unique,
    });
  };

  return (
    <>
      {canList &&
        (isLoading ? (
          <SkeletonList count={2} />
        ) : indexes.length === 0 ? (
          <Text as="p">No indexes found.</Text>
        ) : (
          indexes.map((index) => (
            <div key={index.name} className="mb-1">
              <Flex align="center" gap="2">
                {canDrop && (
                  <IconButton
                    type="button"
                    mode="destructive"
                    variant="outline"
                    size="1"
                    icon={FaTrash}
                    title={`Remove index ${index.name}`}
                    onClick={() =>
                      destructiveConfirm({
                        resourceName: index.name,
                        resourceType: 'Index',
                        onConfirm: async () => {
                          try {
                            await dropIndex({
                              source: { name: source.name, kind: source.kind },
                              table: table.table,
                              indexName: index.name,
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
                  <strong>{index.name}</strong> &middot; {index.type} &middot;{' '}
                  {index.columns.join(', ')}
                </Text>
              </Flex>
            </div>
          ))
        ))}

      {canAdd && (
        <div className="mt-2">
          <Button
            type="button"
            size="sm"
            mode="default"
            leftIcon={FaPlus}
            onClick={() => setIsAddDialogOpen(true)}
          >
            Add Index
          </Button>
        </div>
      )}

      {canAdd && isAddDialogOpen && (
        <IndexForm
          title="Add Index"
          submitLabel="Add Index"
          defaultValues={emptyIndexFormValues}
          columnOptions={columnOptions}
          isPending={isPending}
          onSubmit={onSubmit}
          onClose={() => setIsAddDialogOpen(false)}
        />
      )}
    </>
  );
};

export default Indexes;
