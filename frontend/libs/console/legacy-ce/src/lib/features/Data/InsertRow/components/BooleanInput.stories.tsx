import { Meta } from '@storybook/react-webpack5';
import { expect, userEvent, within } from 'storybook/test';
import { BooleanInput } from './BooleanInput';

export default {
  title: 'Data/Insert Row/components/BooleanInput',
  component: BooleanInput,
  parameters: {},
} as Meta<typeof BooleanInput>;

export const Base = {
  args: {
    // onCheckedChange: action('onCheckedChange'),
    name: 'isActive',
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(await canvas.findByText('true'));

    await expect(args.onCheckedChange).toHaveBeenCalledWith(true);

    await userEvent.click(await canvas.findByText('false'));
    await expect(args.onCheckedChange).toHaveBeenCalledWith(false);
  },
};
