import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { within, userEvent, screen } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { Button } from '../Button';

import { DropdownButton } from './DropdownButton';

export default {
  title: 'components/Dropdown Button 🧬',
  parameters: {
    docs: { source: { type: 'code' } },
    chromatic: { disableSnapshot: true },
  },
  decorators: [
    Story => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
  component: DropdownButton,
  argTypes: {
    onClick: { action: true },
  },
} as Meta<typeof DropdownButton>;

export const Default: StoryObj<typeof Button> = {
  render: ({ onClick }) => (
    <DropdownButton
      data-testid="dropdown-button"
      items={[
        [
          <span onClick={onClick}>Action</span>,
          <span onClick={onClick} className="text-red-600">
            Destructive Action
          </span>,
        ],
        [<span onClick={onClick}>Another action</span>],
      ]}
    >
      The DropdownButton label
    </DropdownButton>
  ),

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    // click the trigger
    userEvent.click(canvas.getByTestId('dropdown-button'));
    // the menu is visible
    expect(screen.getByText('Another action')).toBeVisible();
    // click the item
    userEvent.click(screen.getByText('Another action'));
    // the menu is not visible
    expect(screen.queryByText('Another action')).not.toBeInTheDocument();
    // the action is called
    expect(args.onClick).toHaveBeenCalled();
  },
};

export const ApiPlayground: StoryObj<typeof Button> = {
  render: args => (
    <DropdownButton
      items={[
        ['Action', <span className="text-red-600">Destructive Action</span>],
        ['Another action'],
      ]}
      {...args}
    >
      The DropdownButton label
    </DropdownButton>
  ),
};

export const Disabled: StoryObj<typeof Button> = {
  render: args => (
    <DropdownButton
      items={[
        ['Action', <span className="text-red-600">Destructive Action</span>],
        ['Another action'],
      ]}
      {...args}
      disabled
    >
      The DropdownButton label
    </DropdownButton>
  ),

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    // click the trigger
    userEvent.click(canvas.getByText('The DropdownButton label'));
    // the menu is not visible (please note that this test makes sense only if there is another test
    // that checks that the menu is visible when the button is enabled. Otherwise if the dropdown opens
    // after a millisecond, this test could go green even if then the menu appears)
    expect(screen.queryByText('Another action')).not.toBeInTheDocument();
  },
};
