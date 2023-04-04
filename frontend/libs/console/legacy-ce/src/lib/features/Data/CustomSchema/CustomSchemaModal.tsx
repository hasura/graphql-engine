import { Dialog } from '../../../new-components/Dialog';
import React from 'react';
import { CustomSchemaForm } from './CustomSchemaForm';
import { CustomSchemaFormVals } from './types';

export type CustomSchemaModalProps = {
  tableName: string;
  jsonSchema?: string;
  graphqlSchema?: string;
  onSubmit: (data: CustomSchemaFormVals) => void;
  onClose: () => void;
  isLoading: boolean;
  show?: boolean;
  dialogDescription?: string;
  callToAction?: string;
  callToActionLoadingText?: string;
  callToDeny?: string;
};

export const CustomSchemaModal: React.FC<CustomSchemaModalProps> = ({
  tableName,
  jsonSchema,
  graphqlSchema,
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
      titleTooltip="Customize tracked objects and their GraphQL API representations."
    >
      <CustomSchemaForm
        jsonSchema={jsonSchema}
        graphqlSchema={graphqlSchema}
        onClose={onClose}
        {...formProps}
      />
    </Dialog>
  );
};
