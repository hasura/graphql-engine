import { StoryObj, Meta } from '@storybook/react-webpack5';
import { fn } from 'storybook/test';
import { action } from 'storybook/actions';
import { z } from 'zod';

import { RequestHeaders, RequestHeadersProps } from './RequestHeader';
import { requestHeadersSchema } from './schema';
import { Button } from '../../Button';
import { SimpleForm } from '../../Form';

export default {
  title: 'components/header/RequestHeader',
  parameters: {
    docs: {
      description: {
        component: `A react-hook-form field array for entering a list of static key/value request headers. Must be rendered inside a \`SimpleForm\` (or another react-hook-form provider).`,
      },
      source: { type: 'code' },
    },
  },
  component: RequestHeaders,
} as Meta<typeof RequestHeaders>;

const schema = z.object({
  headers: requestHeadersSchema,
});

interface Props extends RequestHeadersProps {
  onSubmit: (data: unknown) => void;
}

export const ApiPlayground: StoryObj<Props> = {
  render: (args) => (
    <SimpleForm
      options={{
        defaultValues: {
          headers: [{ name: 'x-hasura-role', value: 'admin' }],
        },
      }}
      onSubmit={args.onSubmit}
      schema={schema}
    >
      <>
        <RequestHeaders name={args.name} addButtonText={args.addButtonText} />
        <Button type="submit">Submit</Button>
      </>
    </SimpleForm>
  ),

  args: {
    name: 'headers',
    addButtonText: 'Add',
    onSubmit: fn().mockImplementation(action('submit')),
  },

  name: '⚙️ API',
};

export const Basic: StoryObj<Props> = {
  render: () => (
    <SimpleForm
      options={{
        defaultValues: {
          headers: [
            { name: 'x-hasura-role', value: 'admin' },
            { name: 'x-hasura-user', value: '{{HASURA_USER}}' },
          ],
        },
      }}
      onSubmit={action('submit')}
      schema={schema}
    >
      <RequestHeaders name="headers" />
    </SimpleForm>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateEmpty: StoryObj<Props> = {
  render: () => (
    <SimpleForm
      options={{ defaultValues: { headers: [] } }}
      onSubmit={action('submit')}
      schema={schema}
    >
      <RequestHeaders name="headers" />
    </SimpleForm>
  ),

  name: '🔁 State - Empty',

  parameters: {
    docs: {
      description: {
        story: `With no headers, only the "Add" button is shown — the column labels are hidden until there is at least one row.`,
      },
      source: { state: 'open' },
    },
  },
};
