import { SimpleForm, Button } from '@hasura/shared/ui';

import { StoryFn, Meta } from '@storybook/react-webpack5';
import { z } from 'zod';
import { databaseUrlSchema } from '../schema';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import { DatabaseUrl } from './DatabaseUrl';

export default {
  component: DatabaseUrl,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof DatabaseUrl>;

export const DatabaseUrlDefaultView: StoryFn<typeof DatabaseUrl> = () => (
  <SimpleForm
    onSubmit={(data) => console.log(data)}
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
