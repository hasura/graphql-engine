// Button.stories.ts|tsx
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { StoryObj, Meta } from '@storybook/react';
import { ModifyTable } from './ModifyTable';
import { handlers } from './mock/handlers';

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  component: ModifyTable,

  decorators: [ReactQueryDecorator()],
} as Meta<typeof ModifyTable>;

export const Primary: StoryObj<typeof ModifyTable> = {
  args: {
    table: ['Customer'],
    dataSourceName: 'sqlite',
    tableName: 'Customer',
  },
};

export const TestData = {
  args: {
    table: ['Customer'],
    dataSourceName: 'sqlite',
    tableName: 'Customer',
  },

  parameters: {
    msw: {
      handlers: handlers(),
    },
  },

  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    const dataTypeBadge = await c.findByTestId(
      `CustomerId-ui-data-type`,
      {},
      { timeout: 3000 }
    );
    const hiddenDataInput = await c.findByTestId('CustomerId-data');
    const correctDataType = await hiddenDataInput.dataset['dataType'];
    await expect(correctDataType).toBeTruthy();
    await expect(dataTypeBadge).toHaveTextContent(correctDataType as string);
  },
};
