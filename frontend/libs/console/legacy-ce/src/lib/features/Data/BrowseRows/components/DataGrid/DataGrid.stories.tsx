import { ReactQueryDecorator } from '@hasura/shared/testing';
import { StoryObj, StoryFn, Meta } from '@storybook/react-webpack5';
import { expect, userEvent, waitFor, within } from 'storybook/test';
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
    <DataGrid
      table={['Album']}
      source={{
        name: 'sqlite_test',
        kind: 'sqlite',
        tables: [],
        configuration: {},
      }}
    />
  );
};

export const Testing: StoryObj<typeof DataGrid> = {
  render: () => {
    return (
      <DataGrid
        table={['Album']}
        source={{
          name: 'sqlite_test',
          kind: 'sqlite',
          tables: [],
          configuration: {},
        }}
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
      { timeout: 10000 },
    );

    await userEvent.click(await canvas.findByTestId('@nextPageBtn'));

    await waitFor(
      async () => {
        const firstRow = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
        await expect(firstRow.length).toBe(5);
        await expect(firstRow[0]).toHaveTextContent('11'); // AlbumId
      },
      { timeout: 10000 },
    );

    const firstRow = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
    await expect(firstRow[1]).toHaveTextContent('Out Of Exile');
    await expect(firstRow[2]).toHaveTextContent('8'); // ArtistId
    await expect(firstRow[3]).toHaveTextContent('View');
    await expect(firstRow[4]).toHaveTextContent('View');
  },
};
