import React from 'react';
import z from 'zod';
import { Dialog } from '../../../../../new-components/Dialog';
import {
  CheckboxesField,
  InputField,
} from '../../../../../new-components/Form';

export const modalSchema = z.object({
  tableName: z.string(),
  methods: z
    .enum(['READ', 'READ ALL', 'CREATE', 'UPDATE', 'DELETE'])
    .array()
    .nonempty({ message: 'Choose at least one method' }),
});

export const RestEndpointModal = () => {
  return (
    <Dialog
      hasBackdrop
      title="Auto-Create REST Endpoints"
      onClose={() => {}}
      description="One-click to create REST endpoint from selected table"
      footer={<Dialog.Footer callToAction="Create" onSubmit={() => {}} />}
    >
      <div className="p-md">
        <InputField
          name="tableName"
          label="Table Name"
          disabled
          className="w-2/3"
        />

        <CheckboxesField
          description="Each selected method will generate one endpoint"
          name="methods"
          label="Methods *"
          options={[
            { value: 'READ', label: 'READ' },
            { value: 'READ ALL', label: 'READ ALL' },
            { value: 'CREATE', label: 'CREATE' },
            { value: 'UPDATE', label: 'UPDATE' },
            { value: 'DELETE', label: 'DELETE' },
          ]}
          orientation="horizontal"
        />
      </div>
    </Dialog>
  );
};
