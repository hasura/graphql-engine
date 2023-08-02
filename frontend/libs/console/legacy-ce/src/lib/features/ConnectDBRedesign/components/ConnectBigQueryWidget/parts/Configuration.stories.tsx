import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { StoryFn, Meta } from '@storybook/react';
import { z } from 'zod';

import { Configuration } from './Configuration';

export default {
  component: Configuration,
} as Meta<typeof Configuration>;

export const Primary: StoryFn<typeof Configuration> = () => (
  <SimpleForm
    onSubmit={data => console.log(data)}
    schema={z.any()}
    options={{
      defaultValues: {
        details: {
          databaseUrl: {
            connectionType: 'databaseUrl',
          },
        },
      },
    }}
  >
    <Configuration name="connectionInfo" />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
