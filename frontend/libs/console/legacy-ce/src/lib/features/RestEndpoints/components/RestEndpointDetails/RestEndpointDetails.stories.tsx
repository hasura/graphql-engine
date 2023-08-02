import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { within, userEvent, waitFor } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import {
  RestEndpointDetails,
  RestEndpointDetailsProps,
} from './RestEndpointDetails';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../../../mocks/metadata.mock';
import { rest } from 'msw';

const meta = {
  title: 'Features/REST endpoints/Rest Endpoint Details',
  component: RestEndpointDetails,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: [
      ...handlers({ delay: 0 }),
      rest.post('**/api/rest/test/1', (req, res, ctx) =>
        res(
          ctx.delay(0),
          ctx.json({
            data: {
              update_user_by_pk: {
                id: 1,
                name: 'Hasura',
              },
            },
          })
        )
      ),
    ],
  },
  argTypes: {},
} satisfies Meta<typeof RestEndpointDetails>;

export default meta;

export const Default: StoryObj<RestEndpointDetailsProps> = {
  render: args => {
    return <RestEndpointDetails {...args} name="MyQuery" />;
  },
};

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await waitFor(() => {
    expect(canvas.queryByText('Run Request')).toBeEnabled();
  });

  // await expect(screen.queryByText('user')).toBeInTheDocument();

  // // toggle the user permission and check the success notification
  // await userEvent.click(canvas.getByTestId('user'));
  // await expect(
  //   await canvas.findByText(`Allow list permissions updated`)
  // ).toBeInTheDocument();

  // Add new role
  await userEvent.type(canvas.getByTestId('header-name-0'), 'Authorization');

  await userEvent.type(canvas.getByTestId('header-value-0'), 'Bearer 123');
  await userEvent.click(canvas.getByText('Add Header'));

  await userEvent.type(canvas.getByTestId('variable-id'), '1');

  await userEvent.type(
    canvas.getByTestId('variable-object'),
    '{"object": {"name": "Hasura"}}'
  );

  await userEvent.click(canvas.getByText('Run Request'));
};
