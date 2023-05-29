import { Meta } from '@storybook/react';
import { handlers } from '../../../mocks/metadata.mock';
import { action } from '@storybook/addon-actions';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { BooleanInput } from './BooleanInput';

export default {
  title: 'Data/Insert Row/components/BooleanInput',
  component: BooleanInput,
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof BooleanInput>;

export const Base = {
  args: {
    onCheckedChange: action('onCheckedChange'),
    name: 'isActive',
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    userEvent.click(await canvas.findByText('true'));

    expect(args.onCheckedChange).toHaveBeenCalledWith(true);

    userEvent.click(await canvas.findByText('false'));
    expect(args.onCheckedChange).toHaveBeenCalledWith(false);
  },
};
