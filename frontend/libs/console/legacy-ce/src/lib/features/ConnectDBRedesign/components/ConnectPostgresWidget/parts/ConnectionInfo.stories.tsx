import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { StoryFn, Meta } from '@storybook/react';
import { z } from 'zod';

import { ConnectionInfo } from './ConnectionInfo';

export default {
  component: ConnectionInfo,
} as Meta<typeof ConnectionInfo>;

export const Primary: StoryFn<typeof ConnectionInfo> = () => (
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
    <ConnectionInfo name="connectionInfo" hideOptions={[]} />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
