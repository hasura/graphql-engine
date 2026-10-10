import React from 'react';
import { SimpleForm, Button } from '@hasura/shared/ui';
import { z } from 'zod';
import { action } from 'storybook/actions';

import { StoryFn, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
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
