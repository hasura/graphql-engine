import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { action } from 'storybook/actions';

import { PaginationOffset } from './index';

export default {
  title: 'components/PaginationOffset',
  parameters: {
    docs: {
      description: {
        component: `A prev/next pagination control (no page numbers) with a page-size selector, used for tables where the total row count is unknown.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
  component: PaginationOffset,
} as Meta<typeof PaginationOffset>;

export const ApiPlayground: StoryObj<typeof PaginationOffset> = {
  args: {
    offset: 0,
    limit: 10,
    rows: Array(10).fill({}),
    changePage: action('changePage'),
    changePageSize: action('changePageSize'),
  },

  name: '⚙️ API',
};

export const Playground: StoryObj<typeof PaginationOffset> = {
  render: () => {
    const [offset, setOffset] = React.useState(0);
    const [limit, setLimit] = React.useState(10);
    const totalRows = 42;
    const rows = Array(Math.max(0, Math.min(limit, totalRows - offset))).fill(
      {},
    );
    return (
      <PaginationOffset
        offset={offset}
        limit={limit}
        rows={rows}
        changePage={(page) => setOffset(page * limit)}
        changePageSize={setLimit}
      />
    );
  },

  name: '🧰 Basic - Playground',

  parameters: {
    docs: {
      description: {
        story: `An interactive example backed by a fixed ${42}-row dataset, so \`Next\` disables once the last page is reached.`,
      },
      source: { state: 'open' },
    },
  },
};

export const StateFirstPage: StoryObj<typeof PaginationOffset> = {
  render: () => (
    <PaginationOffset
      offset={0}
      limit={10}
      rows={Array(10).fill({})}
      changePage={action('changePage')}
      changePageSize={action('changePageSize')}
    />
  ),

  name: '🔁 State - First page (Prev disabled)',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateLastPage: StoryObj<typeof PaginationOffset> = {
  render: () => (
    <PaginationOffset
      offset={20}
      limit={10}
      rows={Array(4).fill({})}
      changePage={action('changePage')}
      changePageSize={action('changePageSize')}
    />
  ),

  name: '🔁 State - Last page (Next disabled)',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
