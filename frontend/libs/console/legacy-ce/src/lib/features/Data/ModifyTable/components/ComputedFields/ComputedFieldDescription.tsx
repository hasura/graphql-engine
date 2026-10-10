import { useState } from 'react';
import {
  Badge,
  DestructiveDialogCascade,
  getDestructiveDescription,
  hasuraToast,
  IconButton,
  showErrorNotification,
  Text,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { TbMathFunction } from 'react-icons/tb';
import { functionDisplayName } from '@hasura/metadata/helpers';
import { useDropComputedField } from '@hasura/metadata/api';
import {
  MetadataTable,
  PostgresComputedField,
  Source,
} from '@hasura/shared/types';
import { FaEdit } from 'react-icons/fa';
import { FaTrash } from 'react-icons/fa6';

type ComputedFieldDescriptionProps = {
  source: Source;
  table: MetadataTable;
  computedField: PostgresComputedField;
  onEdit: (computedField: PostgresComputedField) => void;
};

export const ComputedFieldDescription = ({
  source,
  table,
  computedField,
  onEdit,
}: ComputedFieldDescriptionProps) => {
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false);
  const { mutate: dropComputedField, isPending } = useDropComputedField();

  const onDelete = (cascade: boolean) => {
    return dropComputedField(
      {
        source,
        table: table.table,
        name: computedField.name,
        cascade,
      },
      {
        onSuccess: () => {
          setIsDeleteDialogOpen(false);
          hasuraToast({
            title: 'Success!',
            message: 'Computed field dropped successfully',
            type: 'success',
          });
        },
        onError: (error: Error) => {
          showErrorNotification({
            title: 'Error while dropping the computed field',
            error: error?.message,
          });
        },
      },
    );
  };

  return (
    <Flex gap="4" align="center" className="mb-2">
      <Flex gap="2">
        <IconButton
          mode="default"
          size="1"
          title="Edit"
          icon={FaEdit}
          onClick={() => onEdit(computedField)}
        />
        <IconButton
          mode="destructive"
          variant="outline"
          size="1"
          icon={FaTrash}
          title="Remove"
          onClick={() => setIsDeleteDialogOpen(true)}
        />
      </Flex>

      <div>
        <Text weight="bold">{computedField.name}</Text>
        {!!computedField.comment && (
          <Text className="italic block">{computedField.comment}</Text>
        )}
      </div>

      <Badge color="gray">
        <Flex align="center" gap="1">
          <TbMathFunction />
          {functionDisplayName({
            qualifiedFunction: computedField.definition.function,
          })}
        </Flex>
      </Badge>

      {isDeleteDialogOpen && (
        <DestructiveDialogCascade
          title="Remove computed field"
          onClose={() => setIsDeleteDialogOpen(false)}
          onConfirm={onDelete}
          loading={isPending}
        >
          {getDestructiveDescription({
            destroyTerm: 'remove',
            resourceName: computedField.name,
            resourceType: 'computed field',
          })}
        </DestructiveDialogCascade>
      )}
    </Flex>
  );
};
