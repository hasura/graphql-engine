import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { action } from '@storybook/addon-actions';
import { z } from 'zod';
import {
  RequestHeadersSelector,
  RequestHeadersSelectorProps,
  requestHeadersSelectorSchema,
} from '.';
import { Button } from '../Button';
import { SimpleForm } from '../Form';
import { jest } from '@storybook/jest';

export default {
  title: 'components/Request Headers Selector',
  component: RequestHeadersSelector,
  argTypes: {
    onSubmit: { action: true },
  },
} as Meta<typeof RequestHeadersSelector>;

const schema = z.object({
  headers: requestHeadersSelectorSchema,
});

interface Props extends RequestHeadersSelectorProps {
  onSubmit: jest.Mock<void, [Record<string, unknown>]>;
}

export const Primary: StoryObj<Props> = {
  render: args => {
    return (
      <SimpleForm
        options={{
          defaultValues: {
            headers: [
              { name: 'x-hasura-role', value: 'admin', type: 'from_value' },
              { name: 'x-hasura-user', value: 'HASURA_USER', type: 'from_env' },
            ],
          },
        }}
        onSubmit={args.onSubmit}
        schema={schema}
      >
        <>
          <RequestHeadersSelector name={args.name} />
          <Button type="submit">Submit</Button>
        </>
      </SimpleForm>
    );
  },

  args: {
    name: 'headers',
    onSubmit: jest.fn().mockImplementation(action('submit')),
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    const addNewRowButton = canvas.getByText('Add');

    // Add a third header
    await userEvent.click(addNewRowButton);
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[2].name',
      }),
      'x-hasura-name'
    );
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[2].value',
      }),
      'hasura_user'
    );

    // Add a fourth header
    await userEvent.click(addNewRowButton);
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[3].name',
      }),
      'x-hasura-id'
    );
    await userEvent.selectOptions(
      canvas.getByRole('combobox', {
        name: 'headers[3].type',
      }),
      'from_env'
    );
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[3].value',
      }),
      'HASURA_ENV_ID'
    );

    await userEvent.click(canvas.getByText('Submit'));

    const submittedHeaders: z.infer<typeof schema> = {
      headers: [
        { name: 'x-hasura-role', type: 'from_value', value: 'admin' },
        {
          name: 'x-hasura-user',
          type: 'from_env',
          value: 'HASURA_USER',
        },
        {
          name: 'x-hasura-name',
          type: 'from_value',
          value: 'hasura_user',
        },
        {
          name: 'x-hasura-id',
          type: 'from_env',
          value: 'HASURA_ENV_ID',
        },
      ],
    };

    await waitFor(() => expect(args.onSubmit).toHaveBeenCalledTimes(1));

    // ATTENTION: The more idiomatic version of this assertion is:
    //  expect(args.onSubmit).toBeCalledWith(
    // expect.objectContaining({ ...submittedHeaders })
    //  );
    // but at the time of writing, I (Stefano Magni) cannot get why it fails.
    // Hence the need to access mock.calls directly

    const formHeaders = args.onSubmit.mock.calls[0][0];
    expect(formHeaders).toMatchObject(submittedHeaders);
  },
};
