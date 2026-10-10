import { useState } from 'react';
import { Button, IndicatorCard, SkeletonList, Text } from '@hasura/shared/ui';
import { FaPlus } from 'react-icons/fa';
import { useTrackableComputedFunctions } from '@hasura/metadata/data-source';
import { PostgresComputedField } from '@hasura/shared/types';
import { ModifyTableProps } from '../../types';
import { ComputedFieldDescription } from './ComputedFieldDescription';
import { ComputedFieldDialog } from './ComputedFieldDialog';

type ComputedFieldsProps = ModifyTableProps;

export const ComputedFields = ({ source, table }: ComputedFieldsProps) => {
  const [editingComputedField, setEditingComputedField] = useState<
    PostgresComputedField | undefined
  >();
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false);

  const {
    data: functions = [],
    isLoading,
    isError,
  } = useTrackableComputedFunctions({ source });

  const computedFields = (table.computed_fields ??
    []) as PostgresComputedField[];

  const isDialogOpen = isAddDialogOpen || Boolean(editingComputedField);

  const closeDialog = () => {
    setIsAddDialogOpen(false);
    setEditingComputedField(undefined);
  };

  if (isLoading) return <SkeletonList count={3} />;

  if (isError)
    return (
      <IndicatorCard status="negative" headline="error">
        Unable to fetch functions
      </IndicatorCard>
    );

  return (
    <>
      {computedFields.length === 0 ? (
        <div className="mb-2">
          <Text>No computed fields found.</Text>
        </div>
      ) : (
        computedFields.map((computedField) => (
          <ComputedFieldDescription
            key={computedField.name}
            source={source}
            table={table}
            computedField={computedField}
            onEdit={setEditingComputedField}
          />
        ))
      )}

      <Button
        mode="default"
        size="sm"
        leftIcon={FaPlus}
        onClick={() => setIsAddDialogOpen(true)}
      >
        Add Computed Field
      </Button>

      {isDialogOpen && (
        <ComputedFieldDialog
          source={source}
          table={table}
          functions={functions}
          computedField={editingComputedField}
          onClose={closeDialog}
        />
      )}
    </>
  );
};

export default ComputedFields;
