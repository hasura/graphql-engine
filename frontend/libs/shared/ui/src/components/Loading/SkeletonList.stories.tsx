import { StoryObj, Meta } from '@storybook/react-webpack5';

import { SkeletonList } from './SkeletonList';

export default {
  title: 'components/SkeletonList',
  parameters: {
    docs: {
      description: {
        component: `Renders a list of Radix \`Skeleton\` placeholders, used while content is loading.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
  component: SkeletonList,
} as Meta<typeof SkeletonList>;

export const ApiPlayground: StoryObj<typeof SkeletonList> = {
  args: {
    count: 5,
  },

  name: '⚙️ API',
};

export const Basic: StoryObj<typeof SkeletonList> = {
  render: () => <SkeletonList count={5} />,

  name: '🧰 Basic',

  parameters: {
    docs: {
      description: {
        story: `⚠️ \`SkeletonList\` currently builds its rows with \`Array(count).map(...)\`, which iterates over a sparse array and never invokes the callback — so no \`Skeleton\` rows are rendered. This story documents the intended usage; see [SkeletonList.tsx](./SkeletonList.tsx).`,
      },
      source: { state: 'open' },
    },
  },
};

export const CustomSize: StoryObj<typeof SkeletonList> = {
  render: () => <SkeletonList count={3} width="200px" height="16px" />,

  name: '🎭 Variant - Custom size',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
