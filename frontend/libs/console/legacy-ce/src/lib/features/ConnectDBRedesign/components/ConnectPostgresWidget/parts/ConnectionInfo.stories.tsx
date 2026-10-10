import { Button, SimpleForm } from '@hasura/shared/ui';
import { StoryFn, Meta } from '@storybook/react-webpack5';
import { z } from 'zod';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { ConnectionInfo } from './ConnectionInfo';

export default {
  component: ConnectionInfo,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof ConnectionInfo>;

export const Primary: StoryFn<typeof ConnectionInfo> = () => (
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
    <ConnectionInfo name="connectionInfo" hideOptions={[]} />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
