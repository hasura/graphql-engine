import { StoryObj, Meta } from '@storybook/react-webpack5';
import { expect, screen, userEvent, within } from 'storybook/test';

import { DropdownButton } from './DropdownButton';
import { DropdownMenu } from '../DropdownMenu';

export default {
  title: 'components/dropdown/Dropdown Button 🧬',
  parameters: {
    docs: { source: { type: 'code' } },
    chromatic: { disableSnapshot: true },
  },
  decorators: [
    (Story) => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
  component: DropdownButton,
  argTypes: {
    onClick: { action: true },
  },
} as Meta<typeof DropdownButton>;

export const Default: StoryObj<typeof DropdownButton> = {
  render: () => (
    <DropdownButton
      data-testid="dropdown-button"
      items={[
        <DropdownMenu.Item>Action</DropdownMenu.Item>,
        <DropdownMenu.Item className="text-red-600">
          Destructive Action
        </DropdownMenu.Item>,
        <DropdownMenu.Item>Another action</DropdownMenu.Item>,
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

export const ApiPlayground: StoryObj<typeof DropdownButton> = {
  render: (args) => (
    <DropdownButton
      {...args}
      items={[
        <DropdownMenu.Item>Action</DropdownMenu.Item>,
        <DropdownMenu.Item color="red">Destructive Action</DropdownMenu.Item>,
        <DropdownMenu.Item>Another action</DropdownMenu.Item>,
      ]}
    >
      The DropdownButton label
    </DropdownButton>
  ),
};

export const Disabled: StoryObj<typeof DropdownButton> = {
  render: (args) => (
    <DropdownButton
      {...args}
      items={[
        <DropdownMenu.Item>Action</DropdownMenu.Item>,
        <DropdownMenu.Item color="red">Destructive Action</DropdownMenu.Item>,
        <DropdownMenu.Item>Another action</DropdownMenu.Item>,
      ]}
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
