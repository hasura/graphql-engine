import React from 'react';
import { z } from 'zod';

import { Dialog } from '../../../../../new-components/Dialog';
import {
  Textarea,
  InputField,
  SimpleForm,
  CodeEditorField,
} from '../../../../../new-components/Form';
import { TableColumn } from '../../../../DataSource';

interface RowDialogProps {
  row: Record<string, any>;
  onClose: () => void;
  columns: TableColumn[];
}

const schema = z.object({});

export const RowDialog = ({ onClose, row, columns }: RowDialogProps) => {
  // Add submitting and schema validation when we work on editing columns
  // const onSubmit = (values: Record<string, unknown>) => {};

  const rowSections = Object.entries(row).map(([key, value]) => {
    const columnDataType = columns.find(
      column => column.name === key
    )?.consoleDataType;

    if (columnDataType === 'json')
      return <CodeEditorField name={key} label={key} disabled />;

    if (columnDataType === 'string')
      return <InputField disabled type="text" name={key} label={key} />;

    if (columnDataType === 'number' || columnDataType === 'float')
      return <InputField disabled type="number" name={key} label={key} />;

    if (columnDataType === 'boolean')
      return <Textarea disabled name={key} label={key} />;

    if (columnDataType === 'text')
      return <Textarea disabled name={key} label={key} />;

    return <Textarea disabled name={key} label={key} />;
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
