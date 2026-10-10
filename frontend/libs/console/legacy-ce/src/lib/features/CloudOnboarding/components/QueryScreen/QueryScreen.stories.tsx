import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { expect, userEvent, within } from 'storybook/test';
import { QueryScreen } from './QueryScreen';
import type { Props } from './QueryScreen';

const defaultQuery = `
#
#  An example query:
#  Lookup all customers and their orders based on a foreign key relationship.
#  ┌──────────┐     ┌───────┐
#  │ customer │---->│ order │
#  └──────────┘     └───────┘
#

query lookupCustomerOrder {
  customer {
    id
    first_name
    last_name
    username
    email
    phone
    orders {
      id
      order_date
      product
      purchase_price
      discount_price
    }
  }
}
`;

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/Query Screen',
  component: QueryScreen,
  argTypes: {
    onRunHandler: { action: true },
    onSkipHandler: { action: true },
  },
} as Meta<typeof QueryScreen>;

export const Base: StoryObj<Props> = {
  render: (args) => (
    <QueryScreen
      schemaImage={args.schemaImage}
      onRunHandler={args.onRunHandler}
      onSkipHandler={args.onSkipHandler}
      query={defaultQuery}
    />
  ),

  args: {
    schemaImage:
      'https://raw.githubusercontent.com/hasura/template-gallery/main/postgres/getting-started/diagram.png',
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);
    const runButton = canvas.getByText('Run a Sample Query');
    const skipButton = canvas.getByText('Skip, continue to Console');

    // Expect element renders successfully
    await expect(canvas.getByText(`You're ready to go!`)).toBeVisible();

    // Expect button to be present in the dialog
    await expect(runButton).toBeInTheDocument();
    await expect(runButton).not.toBeDisabled();
    await expect(skipButton).toBeInTheDocument();

    await userEvent.click(runButton);
    await expect(args.onRunHandler).toBeCalledTimes(1);
    await userEvent.click(skipButton);
    await expect(args.onSkipHandler).toBeCalledTimes(1);
  },
};
