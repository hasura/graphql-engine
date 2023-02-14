import { SimpleForm } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { z } from 'zod';

import { ConnectionInfo } from './ConnectionInfo';

export default {
  component: ConnectionInfo,
} as ComponentMeta<typeof ConnectionInfo>;

export const Primary: ComponentStory<typeof ConnectionInfo> = () => (
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
    <ConnectionInfo name="connectionInfo" />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
