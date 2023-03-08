import React from 'react';
import { z } from 'zod';

import { Dialog } from '../../../../../new-components/Dialog';
import {
  Textarea,
  InputField,
  SimpleForm,
} from '../../../../../new-components/Form';

interface RowDialogProps {
  row: Record<string, any>;
  onClose: () => void;
}

const schema = z.object({});

export const RowDialog = ({ onClose, row }: RowDialogProps) => {
  // Add submitting and schema validation when we work on editing columns
  // const onSubmit = (values: Record<string, unknown>) => {};

  const rowSections = Object.entries(row).map(([key, value]) => {
    switch (typeof value) {
      case 'string':
        return <Textarea disabled name={key} label={key} />;

      default:
        return (
          <div>
            <InputField disabled type="text" name={key} label={key} />
          </div>
        );
    }
  });

  return (
    <Dialog hasBackdrop title="Table Row" onClose={onClose}>
      <>
        <SimpleForm
          schema={schema}
          onSubmit={() => {}}
          options={{
            defaultValues: {
              ...row,
            },
          }}
        >
          <div className="p-4">{rowSections}</div>
        </SimpleForm>
      </>
    </Dialog>
  );
};
