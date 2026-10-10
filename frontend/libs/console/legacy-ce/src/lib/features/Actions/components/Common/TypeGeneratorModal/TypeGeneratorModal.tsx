import { Dialog, SimpleForm, DialogFooter } from '@hasura/shared/ui';
import { SchemaType } from './types';
import React from 'react';
import { schema, TypeGeneratorForm } from './TypeGeneratorForm';

interface TypeGeneratorModalProps {
  onInsertTypes: (types: string) => void;
  onClose: () => void;
  isOpen: boolean;
}

export const TypeGeneratorModal = ({
  isOpen,
  onClose,
  onInsertTypes,
}: TypeGeneratorModalProps) => {
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
      size="xxxl"
      open={isOpen}
      footer={
        <DialogFooter
          onSubmit={() => {
            onInsertTypes(
              `${values?.graphqlInput ?? ''}\n${
                values?.graphqlOutput ?? ''
              }`.trim(),
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
      onClose={onClose}
    >
      <div className="py-4">
        <div className="mb-4">
          Generate your GraphQL Types from a sample of your request and response
          body.
        </div>
        <SimpleForm
          options={{
            defaultValues: values,
          }}
          schema={schema}
          onSubmit={() => {}}
        >
          <TypeGeneratorForm setValues={setValues} />
        </SimpleForm>
      </div>
    </Dialog>
  );
};
