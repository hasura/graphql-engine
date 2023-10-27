import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { DataGrid } from './DataGrid';
import { handlers } from '../../__mocks__/handlers.mock';

export default {
  component: DataGrid,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers('http://localhost:8080'),
  },
} as Meta<typeof DataGrid>;

export const Primary: StoryFn<typeof DataGrid> = () => {
  return (
    <DataGrid table={['Album']} dataSourceName="sqlite_test" primaryKeys={[]} />
  );
};

export const Testing: StoryObj<typeof DataGrid> = {
  render: () => {
    return (
      <DataGrid
        table={['Album']}
        dataSourceName="sqlite_test"
        primaryKeys={[]}
      />
    );
  },

  name: '🧪 Test - Pagination',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(
      async () => {
        await canvas.findAllByTestId(/^@table-cell-0-.*$/);
      },
      { timeout: 10000 }
    );

    await userEvent.click(await canvas.findByTestId('@nextPageBtn'));

    await waitFor(
      async () => {
        const firstRow = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
        expect(firstRow.length).toBe(5);
        expect(firstRow[0]).toHaveTextContent('11'); // AlbumId
      },
      { timeout: 10000 }
    );

    const firstRow = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
    expect(firstRow[1]).toHaveTextContent('Out Of Exile');
    expect(firstRow[2]).toHaveTextContent('8'); // ArtistId
    expect(firstRow[3]).toHaveTextContent('View');
    expect(firstRow[4]).toHaveTextContent('View');
  },
};
