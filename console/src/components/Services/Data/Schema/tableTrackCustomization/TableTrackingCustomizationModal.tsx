import { Dialog } from '@/new-components/Dialog';
import React from 'react';
import { useForm } from 'react-hook-form';
import {
  FormValues,
  TableTrackingCustomizationForm,
} from './TableTrackingCustomizationForm';

export type TableTrackingCustomizationModalProps = {
  tableName: string;
  onSubmit: (data: FormValues) => void;
  onClose: () => void;
  isLoading: boolean;
};

export const TableTrackingCustomizationModal: React.FC<TableTrackingCustomizationModalProps> = ({
  tableName,
  onSubmit,
  onClose,
  isLoading,
}) => {
  const methods = useForm<FormValues>({
    defaultValues: {
      custom_name: '',
      select: '',
      select_by_pk: '',
      select_aggregate: '',
      insert: '',
      insert_one: '',
      update: '',
      update_by_pk: '',
      delete: '',
      delete_by_pk: '',
    },
  });

  return (
    <form onSubmit={methods.handleSubmit(onSubmit)}>
      <Dialog
        hasBackdrop
        title={tableName}
        description="Rename your table to resolve a conflicting with an existing GraphQL node."
        onClose={onClose}
        footer={{
          callToAction: 'Customize and Track',
          callToActionLoadingText: 'Sending...',
          callToDeny: 'Cancel',
          onClose,
          isLoading,
        }}
      >
        <>
          <TableTrackingCustomizationForm
            initialTableName={tableName}
            formMethods={methods}
          />
        </>
      </Dialog>
    </form>
  );
};
