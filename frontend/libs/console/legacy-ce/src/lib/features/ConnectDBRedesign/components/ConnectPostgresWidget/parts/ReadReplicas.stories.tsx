import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { z } from 'zod';

import { ReadReplicas } from './ReadReplicas';
import { connectionInfoSchema } from '../schema';

export default {
  component: ReadReplicas,
} as ComponentMeta<typeof ReadReplicas>;

export const Primary: ComponentStory<typeof ReadReplicas> = () => (
  <SimpleForm
    onSubmit={data => console.log(data)}
    schema={z.object({
      readReplicas: z.array(connectionInfoSchema),
    })}
    options={{}}
  >
    <ReadReplicas name="readReplicas" hideOptions={[]} />
    <br />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
