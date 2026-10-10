import { useState } from 'react';
import { Flex, Strong } from '@radix-ui/themes';
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
  useCreateUniqueKey,
  useDropUniqueKey,
  useTableColumns,
  useTableUniqueKeys,
} from '@hasura/metadata/data-source';
import { ModifyTableProps } from '../types';
import {
  emptyUniqueKeyFormValues,
  UniqueKeyForm,
  UniqueKeyFormValues,
} from './UniqueKeyForm';

/**
 * Unique-key editor for an existing table: list / add / remove, each gated on
 * the real driver method (`getUniqueKeys` / `createUniqueKey` /
 * `dropUniqueKey`). PG family only.
 */
export const UniqueKeys = ({ source, table }: ModifyTableProps) => {
  const dbMethods = getDatabaseMethods(source.kind);
  const canList = Boolean(dbMethods.introspection.getUniqueKeys);
  const canAdd = Boolean(dbMethods.modify?.createUniqueKey);
  const canDrop = Boolean(dbMethods.modify?.dropUniqueKey);

  const destructiveConfirm = useDestructiveConfirm();
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false);

  const { data: uniqueKeys = [], isLoading } = useTableUniqueKeys(
    { source, table: table.table },
    { enabled: canList },
  );

  const { data: columnData } = useTableColumns({ source, table: table.table });
  const columnOptions = (columnData?.columns ?? []).map((c) => ({
    value: c.name,
    label: c.name,
  }));

  const { mutateAsync: dropUniqueKey } = useDropUniqueKey();

  const { mutate: createUniqueKey, isPending } = useCreateUniqueKey({
    onSuccess: () => setIsAddDialogOpen(false),
  });

  const onSubmit = (values: UniqueKeyFormValues) => {
    createUniqueKey({
      source: { name: source.name, kind: source.kind },
      table: table.table,
      constraintName: values.name,
      columns: values.columns,
    });
  };

  return (
    <div>
      {canList &&
        (isLoading ? (
          <SkeletonList count={2} />
        ) : uniqueKeys.length === 0 ? (
          <div className="mb-2">
            <Text as="p">No unique keys found.</Text>
          </div>
        ) : (
          <div className="mb-3">
            {uniqueKeys.map((uk) => (
              <Flex
                key={uk.constraintName}
                align="center"
                className="mb2"
                gap="2"
              >
                {canDrop && (
                  <IconButton
                    type="button"
                    mode="destructive"
                    variant="outline"
                    size="1"
                    icon={FaTrash}
                    title={`Remove unique key ${uk.constraintName}`}
                    onClick={() =>
                      destructiveConfirm({
                        resourceName: uk.constraintName,
                        resourceType: 'Unique Key',
                        onConfirm: async () => {
                          try {
                            await dropUniqueKey({
                              source: { name: source.name, kind: source.kind },
                              table: table.table,
                              constraintName: uk.constraintName,
                              columns: uk.columns,
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
                  <Strong>{uk.constraintName}</Strong> &middot;{' '}
                  {uk.columns.join(', ')}
                </Text>
              </Flex>
            ))}
          </div>
        ))}

      {canAdd && (
        <Button
          type="button"
          mode="default"
          size="1"
          leftIcon={FaPlus}
          onClick={() => setIsAddDialogOpen(true)}
        >
          Add Unique Key
        </Button>
      )}

      {canAdd && isAddDialogOpen && (
        <UniqueKeyForm
          title="Add Unique Key"
          submitLabel="Add"
          columnOptions={columnOptions}
          defaultValues={emptyUniqueKeyFormValues}
          isPending={isPending}
          onSubmit={onSubmit}
          onClose={() => setIsAddDialogOpen(false)}
        />
      )}
    </div>
  );
};

export default UniqueKeys;
