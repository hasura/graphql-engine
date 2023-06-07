import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { StoryFn, Meta } from '@storybook/react';
import { z } from 'zod';
import { DatabaseUrl } from './DatabaseUrl';
import { databaseUrlSchema } from '../schema';

export default {
  component: DatabaseUrl,
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
