// Button.stories.ts|tsx
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import React from 'react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ModifyTable } from './ModifyTable';
import { handlers } from './mock/handlers';

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  component: ModifyTable,

  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof ModifyTable>;

export const Primary: ComponentStory<typeof ModifyTable> = args => (
  <ModifyTable {...args} />
);

Primary.args = {
  table: ['Customer'],
  dataSourceName: 'sqlite',
  tableName: 'Customer',
};

export const TestData = Primary.bind({});

TestData.args = {
  table: ['Customer'],
  dataSourceName: 'sqlite',
  tableName: 'Customer',
};

TestData.parameters = {
  msw: {
    handlers: handlers(),
  },
};

TestData.play = async ({ canvasElement }) => {
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
};
