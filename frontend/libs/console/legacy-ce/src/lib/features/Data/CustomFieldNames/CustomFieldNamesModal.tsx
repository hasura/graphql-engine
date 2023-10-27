import { MetadataTableConfig } from '../../hasura-metadata-types';
import { Dialog } from '../../../new-components/Dialog';
import React from 'react';
import { CustomFieldNamesForm } from './CustomFieldNamesForm';
import { CustomFieldNamesFormVals } from './types';

export type CustomFieldNamesModalProps = {
  tableName: string;
  onSubmit: (
    data: CustomFieldNamesFormVals,
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
  source: string;
};

export const CustomFieldNamesModal: React.FC<CustomFieldNamesModalProps> = ({
  tableName,
  onClose,
  show = true,
  dialogDescription,
  ...formProps
}) => {
  if (!show) return null;

  return (
    <Dialog
      hasBackdrop
      title={tableName}
      description={dialogDescription}
      onClose={onClose}
      titleTooltip="Customize table name and root fields for GraphQL operations."
    >
      <CustomFieldNamesForm
        initialTableName={tableName}
        onClose={onClose}
        {...formProps}
      />
    </Dialog>
  );
};
