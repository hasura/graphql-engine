import { TrackingTableFormValues } from '@/components/Services/Data/Schema/tableTrackCustomization/types';
import { buildConfigFromFormValues } from '@/components/Services/Data/Schema/tableTrackCustomization/utils';
import { MetadataTableConfig } from '@/features/MetadataAPI';
import { Dialog } from '@/new-components/Dialog';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TableTrackingCustomizationForm } from './TableTrackingCustomizationForm';

export type TableTrackingCustomizationModalProps = {
  tableName: string;
  onSubmit: (
    data: TrackingTableFormValues,
    configuration: MetadataTableConfig
  ) => void;
  onClose: () => void;
  isLoading: boolean;
  show?: boolean;
};

export const TableTrackingCustomizationModal: React.FC<TableTrackingCustomizationModalProps> =
  ({ tableName, onSubmit, onClose, isLoading, show = true }) => {
    const methods = useForm<TrackingTableFormValues>({
      defaultValues: {
        custom_name: '',
        select: '',
        select_by_pk: '',
        select_aggregate: '',
        select_stream: '',
        insert: '',
        insert_one: '',
        update: '',
        update_by_pk: '',
        delete: '',
        delete_by_pk: '',
      },
    });

    const handleSubmit = (data: TrackingTableFormValues) => {
      onSubmit(data, buildConfigFromFormValues(data));
    };

    return (
      <>
        {show && (
          <form onSubmit={methods.handleSubmit(handleSubmit)}>
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
        )}
      </>
    );
  };
