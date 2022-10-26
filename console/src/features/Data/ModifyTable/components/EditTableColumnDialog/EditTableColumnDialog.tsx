import { Table } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { Dialog } from '@/new-components/Dialog';
import { InputField, UpdatedForm } from '@/new-components/Form';
import React from 'react';
import { schema } from './schema';
import { useUpdateTableColumn } from '../../hooks/useUpdateTableColumn';
import { ModifyTableColumn } from '../../types';

interface EditTableColumnDialogProps {
  onClose: () => void;
  column: ModifyTableColumn;
  table: Table;
  dataSourceName: string;
}

export const EditTableColumnDialog = (props: EditTableColumnDialogProps) => {
  const { onClose, column, table, dataSourceName } = props;

  const { updateTableColumn, isLoading: isSaveInProgress } =
    useUpdateTableColumn({ table, dataSourceName });
  return (
    <UpdatedForm
      schema={schema}
      onSubmit={data => {
        updateTableColumn({
          column,
          updatedConfig: data,
          customOnSuccess: onClose,
        });
      }}
      options={{
        defaultValues: {
          custom_name: column.config?.custom_name ?? '',
          comment: column.config?.comment ?? '',
        },
      }}
    >
      {() => (
        <Dialog
          size="md"
          description="Edit the columns settings"
          title="Modify Column"
          hasBackdrop
          onClose={onClose}
          footer={
            <div className="bg-white p-2 justify-end border flex">
              <Button type="submit" isLoading={isSaveInProgress}>
                Submit
              </Button>
            </div>
          }
        >
          <div className="m-4">
            <InputField
              tooltip="Add a comment for your table column"
              label="Comment"
              name="comment"
              placeholder="Add a comment"
            />
            <InputField
              label="Custom GraphQL field name"
              name="custom_name"
              placeholder="Enter GraphQL field name"
              tooltip="Add a custom GQL field name for table column"
            />
          </div>
        </Dialog>
      )}
    </UpdatedForm>
  );
};
