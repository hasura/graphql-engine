import React, { useState } from 'react';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  IconButton,
  IndicatorCard,
  SkeletonList,
  Text,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
import { ModifyTableProps } from '../types';
import { ForeignKeyDescription } from './ForeignKeyDescription';
import {
  generateForeignKeyLabel,
  getDatabaseMethods,
  useDropForeignKey,
  useTableForeignKeys,
} from '@hasura/metadata/data-source';
import { ForeignKeyAddForm } from './ForeignKeyAddForm';
import { ForeignKeyEditForm } from './ForeignKeyEditForm';

type ForeignKeysProps = ModifyTableProps;

export const ForeignKeys: React.FC<ForeignKeysProps> = (props) => {
  const { source, table } = props;
  const databaseMethods = getDatabaseMethods(source.kind);
  const canCreate = Boolean(databaseMethods.modify?.createForeignKey);
  const canDrop = Boolean(databaseMethods.modify?.dropForeignKey);
  const canAlter = Boolean(databaseMethods.modify?.alterForeignKey);

  const [editingIndex, setEditingIndex] = useState<number | null>(null);
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false);

  const destructiveConfirm = useDestructiveConfirm();
  const { mutateAsync: dropForeignKey } = useDropForeignKey();

  const {
    data: foreignKeys = [],
    isLoading,
    isError,
  } = useTableForeignKeys({
    source,
    table: table.table,
  });

  const editingForeignKey =
    editingIndex === null ? undefined : foreignKeys?.[editingIndex];

  if (isLoading || !foreignKeys) return <SkeletonList count={5} />;

  if (isError)
    return (
      <IndicatorCard status="negative" headline="error">
        Unable to fetch foreign keys
      </IndicatorCard>
    );

  return (
    <>
      {foreignKeys.length === 0 ? (
        <Text as="p">No foreign keys found.</Text>
      ) : (
        foreignKeys.map((foreignKey, index) => {
          const label = foreignKey.name ?? generateForeignKeyLabel(foreignKey);
          return (
            <div key={index} className="mb-1">
              <Flex align="center" gap="2">
                {canAlter && (
                  <IconButton
                    mode="default"
                    type="button"
                    size="1"
                    icon={FaEdit}
                    title="Edit"
                    aria-label={`Edit foreign key ${label}`}
                    onClick={() => setEditingIndex(index)}
                  />
                )}
                {canDrop && (
                  <IconButton
                    type="button"
                    mode="destructive"
                    variant="outline"
                    size="1"
                    icon={FaTrash}
                    title="Remove"
                    aria-label={`Remove foreign key ${label}`}
                    onClick={() =>
                      destructiveConfirm({
                        resourceName: label,
                        resourceType: 'Foreign Key',
                        onConfirm: async () => {
                          try {
                            await dropForeignKey({
                              source: {
                                name: source.name,
                                kind: source.kind,
                              },
                              constraintName: foreignKey.name ?? '',
                              from: foreignKey.from,
                              to: foreignKey.to,
                              onUpdate: foreignKey.onUpdate,
                              onDelete: foreignKey.onDelete,
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
                <ForeignKeyDescription foreignKey={foreignKey} />
              </Flex>
            </div>
          );
        })
      )}

      {canCreate && (
        <div className="mt-2">
          <Button
            type="button"
            size="sm"
            mode="default"
            leftIcon={FaPlus}
            onClick={() => setIsAddDialogOpen(true)}
          >
            Add Foreign Key
          </Button>
        </div>
      )}

      {canCreate && isAddDialogOpen && (
        <ForeignKeyAddForm
          source={source}
          table={table.table}
          onClose={() => setIsAddDialogOpen(false)}
        />
      )}

      {canAlter && editingForeignKey && (
        <ForeignKeyEditForm
          source={source}
          table={table.table}
          foreignKey={editingForeignKey}
          onDone={() => setEditingIndex(null)}
        />
      )}
    </>
  );
};
