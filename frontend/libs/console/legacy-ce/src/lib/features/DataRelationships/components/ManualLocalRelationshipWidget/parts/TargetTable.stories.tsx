import React, { useState } from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { z } from 'zod';
import { UpdatedForm } from '@/new-components/Form';
import { TargetTable } from './TargetTable';
import { handlers } from '../__mocks__/localrelationships.mock';

export default {
  title: 'Relationships/TargetTable  🧬',
  component: TargetTable,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Basic: Story<typeof TargetTable> = () => {
  const [formState, updateFormState] = useState<any>('');
  return (
    <>
      <UpdatedForm
        schema={z.object({
          target_name: z.string().min(1, 'Reference source must be provided!'),
          target_table: z.any().transform((value) => {
            try {
              return JSON.parse(value);
            } catch {
              return null;
            }
          }),
        })}
        onSubmit={(data) => {
          updateFormState(data);
        }}
        options={{
          defaultValues: {
            target_name: 'sqlite_test',
          },
        }}
      >
        {() => <TargetTable />}
      </UpdatedForm>
      <div>{formState}</div>
    </>
  );
};
