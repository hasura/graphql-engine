import { StoryFn, Meta } from '@storybook/react';
import { Timeout } from './Timeout';
import { Template } from './Template';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { SimpleForm } from '../../../../../new-components/Form';
import { z } from 'zod';
import { Button } from '../../../../../new-components/Button';

export default {
  component: Timeout,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof Timeout>;

export const BasicView: StoryFn<typeof Timeout> = () => {
  return (
    <SimpleForm
      onSubmit={data => console.log(data)}
      schema={z.object({
        timeout: z
          .number()
          .gte(0, { message: 'Timeout must be a postive number' })
          .optional(),
        template: z.string().optional(),
      })}
    >
      <div className="max-w-3xl">
        <Timeout name="timeout" />
        <Template name="template" />
        <Button type="submit">Submit</Button>
      </div>
    </SimpleForm>
  );
};
