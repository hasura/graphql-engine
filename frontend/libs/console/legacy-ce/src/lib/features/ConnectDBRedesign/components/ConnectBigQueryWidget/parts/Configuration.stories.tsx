import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { z } from 'zod';

import { Configuration } from './Configuration';

export default {
  component: Configuration,
} as ComponentMeta<typeof Configuration>;

export const Primary: ComponentStory<typeof Configuration> = () => (
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
