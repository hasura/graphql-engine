import React from 'react';
import { SimpleForm } from '../../../../../new-components/Form';
import { z } from 'zod';
import { Button } from '../../../../../new-components/Button';

import { StoryFn, Meta } from '@storybook/react';
import { SourceOption, SourceSelect } from './SourceSelect';

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  component: SourceSelect,
} as Meta<typeof SourceSelect>;

const options: SourceOption[] = [
  {
    value: {
      type: 'table',
      dataSourceName: 'chinook',
      table: { name: 'Album', schema: 'public' },
    },
    label: 'chinook / public / Album',
  },
  {
    value: {
      type: 'table',
      dataSourceName: 'chinook',
      table: { name: 'Artist', schema: 'public' },
    },
    label: 'chinook / public / Artist',
  },
  {
    value: {
      type: 'remoteSchema',
      remoteSchema: 'TrevorBlades',
    },
    label: 'TrevorBlades',
  },
];

export const Primary: StoryFn<typeof SourceSelect> = () => (
  <SimpleForm
    schema={z.object({
      test: z.any(),
    })}
    onSubmit={data => {
      console.log(data);
    }}
    options={{
      defaultValues: {
        test: {
          type: 'table',
          dataSourceName: 'chinook',
          table: {
            name: 'Artist',
            schema: 'public',
          },
        },
      },
    }}
  >
    <div>
      <SourceSelect options={options} name="test" label="Select a source" />
      <Button type="submit">Submit</Button>
    </div>
  </SimpleForm>
);
