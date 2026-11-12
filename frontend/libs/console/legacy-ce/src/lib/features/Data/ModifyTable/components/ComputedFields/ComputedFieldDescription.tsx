import {
  Badge,
  DestructiveDialogCascade,
  getDestructiveDescription,
  hasuraToast,
  IconButton,
  showErrorNotification,
  Text,
  useCascadeDialog,
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
  const deleteDialog = useCascadeDialog();
  const { mutate: dropComputedField, isPending } = useDropComputedField();

  const onDelete = (cascade: boolean) => {
    deleteDialog.clearError();
    return dropComputedField(
      {
        source,
        table: table.table,
        name: computedField.name,
        cascade,
      },
      {
        onSuccess: () => {
          deleteDialog.close();
          hasuraToast({
            title: 'Success!',
            message: 'Computed field dropped successfully',
            type: 'success',
          });
        },
        onError: (err: Error) =>
          deleteDialog.handleError(err, (error) =>
            showErrorNotification({
              title: 'Error while dropping the computed field',
              error,
            }),
          ),
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
          onClick={() => deleteDialog.open(true)}
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

      {deleteDialog.isOpen && (
        <DestructiveDialogCascade
          {...deleteDialog.dialogProps}
          title="Remove computed field"
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
