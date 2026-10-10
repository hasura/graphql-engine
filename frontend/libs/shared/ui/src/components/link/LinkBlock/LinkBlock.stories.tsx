import { Meta } from '@storybook/react-webpack5';

import { LinkBlockHorizontal } from './LinkBlockHorizontal';
import { LinkBlockVertical } from './LinkBlockVertical';

export default {
  title: 'components/LinkBlock',
  parameters: {
    docs: {
      description: {
        component: `Decorative connector blocks (a link icon in a circle) used to visually join two rows or columns, e.g. in relationship builders.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
} as Meta;

export const Horizontal = () => (
  <div className="grid grid-cols-2 w-64">
    <div className="h-8 bg-gray-100" />
    <div className="h-8 bg-gray-100" />
    <LinkBlockHorizontal />
  </div>
);

export const Vertical = () => (
  <div className="w-64">
    <div className="h-8 bg-gray-100" />
    <LinkBlockVertical title="Linked to another section" />
    <div className="h-8 bg-gray-100" />
  </div>
);
