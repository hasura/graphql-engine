import React from 'react';
import { Meta } from '@storybook/react';
import { KeyValueListSelector } from './KeyValueListSelector';
import { SimpleForm } from '../Form';
import { z } from 'zod';

const keyValueSchema = z.object({
  checked: z.boolean(),
  key: z.string(),
  value: z.string(),
});

const formSchema = z.object({
  keyValuePairs: z.array(keyValueSchema),
});
export default {
  title: 'components/KeyValuePairsSelector',
  component: KeyValueListSelector,
} as Meta<typeof KeyValueListSelector>;

export const showcase = () => (
  <div className="space-y-4">
    <SimpleForm schema={formSchema} onSubmit={() => {}}>
      <KeyValueListSelector name="list" />
    </SimpleForm>
  </div>
);
