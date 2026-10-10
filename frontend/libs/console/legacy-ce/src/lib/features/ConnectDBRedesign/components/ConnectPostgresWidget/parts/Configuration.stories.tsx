import { Button, SimpleForm } from '@hasura/shared/ui';
import { StoryFn, Meta } from '@storybook/react-webpack5';
import { z } from 'zod';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import { Configuration } from './Configuration';

export default {
  component: Configuration,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof Configuration>;

export const Primary: StoryFn<typeof Configuration> = () => (
  <SimpleForm
    onSubmit={(data) => console.log(data)}
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
    <Configuration name="connectionInfo" hideOptions={[]} />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
