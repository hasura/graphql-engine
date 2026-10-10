// Button.stories.ts|tsx

import type { Meta, StoryObj } from '@storybook/react-webpack5';

import { ModelSummary } from './ModelSummary';
import { useModelCountSummary } from './useModelCountSummary';
import { ReactQueryDecorator } from '@hasura/shared/testing';

const meta: Meta<typeof ModelSummary> = {
  component: ModelSummary,
  decorators: [ReactQueryDecorator()],
};

export default meta;
type Story = StoryObj<typeof ModelSummary>;

/*
 *👇 Render functions are a framework specific feature to allow you control on how the component renders.
 * See https://storybook.js.org/docs/react/api/csf
 * to learn how to use render functions.
 */
export const Primary: Story = {
  render: () => {
    const {
      data: { tablesAndViews = [], logicalModels = [], collections = [] } = {},
      isLoading,
    } = useModelCountSummary();

    if (isLoading) return <>Loading...</>;

    return (
      <ModelSummary
        tablesAndViews={tablesAndViews}
        logicalModels={logicalModels}
        collections={collections}
        isOssMode={true}
      />
    );
  },
};
