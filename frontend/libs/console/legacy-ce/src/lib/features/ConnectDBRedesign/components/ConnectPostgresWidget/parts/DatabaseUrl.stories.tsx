import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { StoryFn, Meta } from '@storybook/react';
import { z } from 'zod';
import { databaseUrlSchema } from '../schema';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';

import { DatabaseUrl } from './DatabaseUrl';

export default {
  component: DatabaseUrl,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof DatabaseUrl>;

export const DatabaseUrlDefaultView: StoryFn<typeof DatabaseUrl> = () => (
  <SimpleForm
    onSubmit={data => console.log(data)}
    schema={z.object({
      databaseUrl: databaseUrlSchema,
    })}
    options={{
      defaultValues: {
        databaseUrl: {
          connectionType: 'databaseUrl',
          url: '',
        },
      },
    }}
  >
    <DatabaseUrl name="databaseUrl" hideOptions={[]} />
    <br />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
