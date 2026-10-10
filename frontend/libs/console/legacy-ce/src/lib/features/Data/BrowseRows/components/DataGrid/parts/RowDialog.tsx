import { z } from 'zod';
import {
  Dialog,
  TextAreaField,
  InputField,
  JsonCodeBlock,
  SimpleForm,
} from '@hasura/shared/ui';

import { TableColumn } from '@hasura/metadata/data-source';

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
      (column) => column.name === key,
    )?.consoleDataType;

    if (columnDataType === 'json')
      return (
        <div key={key}>
          <div className="font-semibold">{key}</div>
          <JsonCodeBlock
            value={
              typeof row[key] === 'string' ? JSON.parse(row[key]) : row[key]
            }
          />
        </div>
      );

    if (columnDataType === 'string')
      return (
        <InputField
          key={key}
          fieldProps={{ disabled: true, type: 'text' }}
          name={key}
          label={key}
        />
      );

    if (
      columnDataType === 'number' ||
      columnDataType === 'float' ||
      columnDataType === 'integer'
    )
      return (
        <InputField
          key={key}
          fieldProps={{ disabled: true, type: 'number' }}
          name={key}
          label={key}
        />
      );

    if (columnDataType === 'boolean')
      return (
        <InputField
          key={key}
          fieldProps={{ disabled: true }}
          name={key}
          label={key}
        />
      );

    if (columnDataType === 'text')
      return <TextAreaField key={key} disabled name={key} label={key} />;

    return <TextAreaField key={key} disabled name={key} label={key} />;
  });

  return (
    <Dialog title="Table Row" onClose={onClose}>
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
