import React from 'react';
import { SimpleForm } from '../../../../new-components/Form';
import { z } from 'zod';
import { action } from '@storybook/addon-actions';
import { Button } from '../../../../new-components/Button';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { TablePicker } from './TablePicker';

export default {
  component: TablePicker,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof TablePicker>;

export const Basic: StoryFn<typeof TablePicker> = () => (
  <SimpleForm
    schema={z.object({
      fromSource: z.object({
        dataSourceName: z.string(),
        table: z.unknown(),
      }),
    })}
    onSubmit={action('onSubmit')}
    options={{
      defaultValues: {
        fromSource: {
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
      <TablePicker type="fromSource" />
      <Button type="submit">Submit</Button>
    </>
  </SimpleForm>
);
