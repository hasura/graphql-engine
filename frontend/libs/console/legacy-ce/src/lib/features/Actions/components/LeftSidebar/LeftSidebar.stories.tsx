import type { Meta, StoryObj } from '@storybook/react-webpack5';
import { expect, within } from 'storybook/test';
import type { Action } from '@hasura/shared/types';
import LeftSidebar from '.';

const makeActions = (names: Array<[string, 'query' | 'mutation']>): Action[] =>
  names.map(
    ([name, type]) => ({ name, definition: { type } }) as unknown as Action,
  );

const longActionName =
  'createCustomerSubscriptionWithPaymentMethodAndBillingAddressValidation';

export default {
  component: LeftSidebar,
  decorators: [
    // as wide as the console's left sidebar
    (Story) => (
      <div style={{ width: 280 }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof LeftSidebar>;

type Story = StoryObj<typeof LeftSidebar>;

export const Default: Story = {
  args: {
    currentAction: 'login',
    actions: makeActions([
      ['login', 'mutation'],
      ['getProfile', 'query'],
      ['signup', 'mutation'],
    ]),
  },
};

export const LongNames: Story = {
  name: '🧪 Test - Long names are truncated',
  args: {
    currentAction: longActionName,
    actions: makeActions([
      ['login', 'mutation'],
      [longActionName, 'mutation'],
      ['getCustomerOrderHistoryWithShipmentTrackingAndInvoiceDetails', 'query'],
      // a name without a natural break point, e.g. in snake case
      ['get_all_active_customer_subscriptions_by_region_and_plan', 'query'],
    ]),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const name = canvas.getByText(longActionName);

    // the name is cut with an ellipsis instead of widening the sidebar, and
    // the full name is still available on hover
    await expect(getComputedStyle(name).textOverflow).toBe('ellipsis');
    await expect(name.scrollWidth).toBeGreaterThan(name.clientWidth);
    await expect(name).toHaveAttribute('title', longActionName);
    await expect(canvasElement.scrollWidth).toBeLessThanOrEqual(
      canvasElement.clientWidth,
    );

    // short names are not affected
    const shortName = canvas.getByText('login');
    await expect(shortName.scrollWidth).toBe(shortName.clientWidth);
  },
};
