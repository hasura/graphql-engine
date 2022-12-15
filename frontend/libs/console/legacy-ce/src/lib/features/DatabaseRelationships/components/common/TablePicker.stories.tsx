import React from 'react';
import { SimpleForm } from '@/new-components/Form';
import { z } from 'zod';
import { action } from '@storybook/addon-actions';
import { Button } from '@/new-components/Button';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { TablePicker } from './TablePicker';

export default {
  title: 'GDC Console/Relationships/components/Table Picker',
  component: TablePicker,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof TablePicker>;

export const Basic: ComponentStory<typeof TablePicker> = () => (
  <SimpleForm
    schema={z.object({
      from: z.object({
        dataSourceName: z.string(),
        table: z.unknown(),
      }),
    })}
    onSubmit={action('onSubmit')}
    options={{
      defaultValues: {
        from: {
          dataSourceName: 'bikes',
          table: {
            name: 'orders',
            schema: 'sales',
          },
        },
      },
    }}
  >
    <>
      <TablePicker name="from" />
      <Button type="submit">Submit</Button>
    </>
  </SimpleForm>
);
