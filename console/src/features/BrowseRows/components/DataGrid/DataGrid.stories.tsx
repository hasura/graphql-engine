import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
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
} as ComponentMeta<typeof DataGrid>;

export const Primary: ComponentStory<typeof DataGrid> = () => {
  return <DataGrid table={['Album']} dataSourceName="sqlite_test" />;
};

export const Testing: ComponentStory<typeof DataGrid> = () => {
  return <DataGrid table={['Album']} dataSourceName="sqlite_test" />;
};

Testing.storyName = '🧪 Test - Pagination';

Testing.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await waitFor(
    async () => {
      userEvent.click(await canvas.findByTestId('@nextPageBtn'));
      const firstRow = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
      expect(firstRow.length).toBe(5);
      expect(firstRow[0]).toHaveTextContent('11'); // AlbumId
    },
    { timeout: 5000 }
  );

  const firstRow = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
  expect(firstRow[1]).toHaveTextContent('Out Of Exile');
  expect(firstRow[2]).toHaveTextContent('8'); // ArtistId
  expect(firstRow[3]).toHaveTextContent('View');
  expect(firstRow[4]).toHaveTextContent('View');
};
