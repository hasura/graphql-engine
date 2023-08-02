import { Dialog } from '../../../../../../new-components/Dialog';
import { SimpleForm } from '../../../../../../new-components/Form';
import { SchemaType } from './types';
import React from 'react';
import { schema, TypeGeneratorForm } from './TypeGeneratorForm';

interface TypeGeneratorModalProps {
  onInsertTypes: (types: string) => void;
  onClose: () => void;
  isOpen: boolean;
}

export const TypeGeneratorModal = (props: TypeGeneratorModalProps) => {
  const { isOpen, onClose, onInsertTypes } = props;
  const [values, setValues] = React.useState<SchemaType>({
    jsonInput: JSON.stringify({
      username: '',
      password: '',
    }),
    graphqlInput: '',
    jsonOutput: JSON.stringify({
      accessToken: '',
    }),
    graphqlOutput: '',
  });

  if (!isOpen) {
    return null;
  }

  return (
    <Dialog
      size="xl"
      footer={
        <Dialog.Footer
          onSubmit={() => {
            onInsertTypes(
              `${values?.graphqlInput ?? ''}\n${
                values?.graphqlOutput ?? ''
              }`.trim()
            );
            onClose();
          }}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Insert Types"
          onSubmitAnalyticsName="actions-tab-generate-types-submit"
          onCancelAnalyticsName="actions-tab-generate-types-cancel"
        />
      }
      title="Type Generator"
      hasBackdrop
      onClose={onClose}
    >
      <div className="px-sm">
        <p className="text-muted mb-6">
          Generate your GraphQL Types from a sample of your request and response
          body.
        </p>
        <SimpleForm
          options={{
            defaultValues: values,
          }}
          className="pl-0 pr-0"
          schema={schema}
          onSubmit={() => {}}
        >
          <TypeGeneratorForm setValues={setValues} />
        </SimpleForm>
      </div>
    </Dialog>
  );
};
