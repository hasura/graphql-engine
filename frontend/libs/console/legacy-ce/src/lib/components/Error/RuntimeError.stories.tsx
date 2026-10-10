import type { Meta, StoryObj } from '@storybook/react-webpack5';
import { expect, fn, userEvent, within } from 'storybook/test';
import RuntimeError from './RuntimeError';

const makeError = (message: string, stack: string) => {
  const error = new Error(message);
  error.stack = stack;
  return error;
};

const shortStack = `TypeError: Cannot read properties of undefined (reading 'name')
    at TableBrowser (webpack://console/libs/console/legacy-ce/src/lib/features/Data/TableBrowser.tsx:42:18)
    at renderWithHooks (webpack://console/node_modules/react-dom/cjs/react-dom.development.js:15486:18)`;

const longStack = [
  "TypeError: Cannot read properties of undefined (reading 'columns')",
  ...Array.from(
    { length: 40 },
    (_, i) =>
      `    at Component${i} (webpack://console/libs/console/legacy-ce/src/lib/features/Data/components/SomeVeryLongComponentName${i}.tsx:${
        100 + i
      }:${10 + i})`,
  ),
].join('\n');

const meta: Meta<typeof RuntimeError> = {
  component: RuntimeError,
  title: 'components / Error / RuntimeError',
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    resetCallback: fn(),
    error: makeError(
      "Cannot read properties of undefined (reading 'name')",
      shortStack,
    ),
  },
};

export default meta;

type Story = StoryObj<typeof RuntimeError>;

export const Default: Story = {};

export const LongStackTrace: Story = {
  args: {
    error: makeError(
      "Cannot read properties of undefined (reading 'columns')",
      longStack,
    ),
  },
};

export const ClickHomeResets: Story = {
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await expect(
      canvas.getByRole('heading', { name: 'Error' }),
    ).toBeInTheDocument();

    await userEvent.click(canvas.getByRole('link', { name: 'Home' }));

    await expect(args.resetCallback).toHaveBeenCalledTimes(1);
  },
};
