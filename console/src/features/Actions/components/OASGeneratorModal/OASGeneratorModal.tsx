import React from 'react';
import { Dialog } from '@/new-components/Dialog';
import { SimpleForm } from '@/new-components/Form';

import { formSchema, OasGeneratorForm } from './OASGeneratorForm';
import { GeneratedAction } from './types';

export interface OasGeneratorModalProps {
  onImport: (output: GeneratedAction) => void;
  onClose: () => void;
}

export const OasGeneratorModal = (props: OasGeneratorModalProps) => {
  const { onClose, onImport } = props;
  const [values, setValues] = React.useState<GeneratedAction>();

  return (
    <Dialog
      size="xl"
      footer={
        <Dialog.Footer
          onSubmit={
            values
              ? () => {
                  onImport(values);
                  onClose();
                }
              : undefined
          }
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Generate Action"
          onSubmitAnalyticsName="action-tab-btn-generate-import-action-from-openapi"
          onCancelAnalyticsName="action-tab-btn-cancel-import-action-from-openapi"
          disabled={!values}
        />
      }
      title="Import OpenAPI endpoint"
      hasBackdrop
      onClose={onClose}
    >
      <div className="px-sm">
        <p className="text-muted mb-6">
          Generate your action from a Open API spec.
        </p>
        <SimpleForm
          className="pl-0 pr-0"
          schema={formSchema}
          onSubmit={() => {}}
        >
          <OasGeneratorForm setValues={setValues} />
        </SimpleForm>
      </div>
    </Dialog>
  );
};
