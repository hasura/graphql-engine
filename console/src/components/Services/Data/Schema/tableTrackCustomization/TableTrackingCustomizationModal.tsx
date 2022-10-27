import { TrackingTableFormValues } from '@/components/Services/Data/Schema/tableTrackCustomization/types';

import { Analytics, REDACT_EVERYTHING } from '@/features/Analytics';
import { MetadataTableConfig } from '@/features/MetadataAPI';
import { Dialog } from '@/new-components/Dialog';
import React from 'react';
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
  dialogDescription?: string;
  callToAction?: string;
  callToActionLoadingText?: string;
  callToDeny?: string;
  currentConfiguration?: MetadataTableConfig;
};

export const TableTrackingCustomizationModal: React.FC<TableTrackingCustomizationModalProps> =
  ({
    tableName,
    onSubmit,
    onClose,
    show = true,
    currentConfiguration,
    dialogDescription,
  }) => {
    return (
      <>
        {show && (
          <Analytics
            name="TableTrackingCustomizationModal"
            {...REDACT_EVERYTHING}
          >
            <Dialog
              hasBackdrop
              title={tableName}
              description={dialogDescription}
              onClose={onClose}
              titleTooltip="Customize table name and root fields for GraphQL operations."
              contentContainer={{
                className: 'mb-[60px]',
              }}
            >
              <TableTrackingCustomizationForm
                initialTableName={tableName}
                currentConfiguration={currentConfiguration}
                onClose={onClose}
                onSubmit={onSubmit}
              />
            </Dialog>
          </Analytics>
        )}
      </>
    );
  };
