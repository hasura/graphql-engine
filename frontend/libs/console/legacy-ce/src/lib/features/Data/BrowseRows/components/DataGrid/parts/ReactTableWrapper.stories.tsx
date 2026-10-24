import { ReactQueryDecorator } from '@hasura/shared/testing';
import { StoryObj, StoryFn, Meta } from '@storybook/react-webpack5';
import { Relationship } from '../../../../../DatabaseRelationships';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { action } from 'storybook/actions';
import { ReactTableWrapper } from './ReactTableWrapper';
import { handlers } from '../../../__mocks__/handlers.mock';

export default {
  component: ReactTableWrapper,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers('http://localhost:8080'),
  },
} as Meta<typeof ReactTableWrapper>;

const mockSource = {
  name: 'sqlite_test',
  kind: 'sqlite' as const,
  tables: [],
  configuration: {},
};

const mockTable = ['Album'];

const mockDataForRows = [
  {
    AlbumId: 1,
    Title: 'For Those About To Rock We Salute You',
    ArtistId: 1,
  },
  { AlbumId: 2, Title: 'Balls to the Wall', ArtistId: 2 },
  { AlbumId: 3, Title: 'Restless and Wild', ArtistId: 2 },
  { AlbumId: 4, Title: 'Let There Be Rock', ArtistId: 1 },
  { AlbumId: 5, Title: 'Big Ones', ArtistId: 3 },
  { AlbumId: 6, Title: 'Jagged Little Pill', ArtistId: 4 },
  { AlbumId: 7, Title: 'Facelift', ArtistId: 5 },
  { AlbumId: 8, Title: 'Warner 25 Anos', ArtistId: 6 },
  { AlbumId: 9, Title: 'Plays Metallica By Four Cellos', ArtistId: 7 },
  { AlbumId: 10, Title: 'Audioslave', ArtistId: 8 },
];

export const Default: StoryFn<typeof ReactTableWrapper> = () => {
  return (
    <ReactTableWrapper
      rows={mockDataForRows}
      isRowsSelectionEnabled
      onRowsSelect={action('onRowsSelect')}
      onRowDelete={action('onRowDelete')}
      tableColumns={[]}
      source={mockSource}
      table={mockTable}
    />
  );
};

export const Basic: StoryObj<typeof ReactTableWrapper> = {
  render: () => {
    return (
      <ReactTableWrapper
        rows={mockDataForRows}
        isRowsSelectionEnabled
        onRowsSelect={action('onRowsSelect')}
        onRowDelete={action('onRowDelete')}
        tableColumns={[]}
        source={mockSource}
        table={mockTable}
      />
    );
  },

  name: '🧪 Test - Basic data with columns',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findAllByTestId(/^@table-row-.*$/)).toHaveLength(
      10,
    );
    await expect(
      await canvas.findAllByTestId(/^@table-cell-0-.*$/),
    ).toHaveLength(3);

    await expect(await canvas.getByTestId('@table-cell-0-1')).toHaveTextContent(
      '1',
    ); // AlbumId
    await expect(await canvas.getByTestId('@table-cell-0-2')).toHaveTextContent(
      'For Those About To Rock We Salute You',
    );
    await expect(await canvas.getByTestId('@table-cell-0-3')).toHaveTextContent(
      '1',
    );
  },
};

export const SelectionDisabled: StoryFn<typeof ReactTableWrapper> = () => {
  return (
    <ReactTableWrapper
      rows={mockDataForRows}
      isRowsSelectionEnabled={false}
      onRowsSelect={action('onRowsSelect')}
      onRowDelete={action('onRowDelete')}
      tableColumns={[]}
      source={mockSource}
      table={mockTable}
    />
  );
};

export const OpenRowFromPrimaryKey: StoryObj<typeof ReactTableWrapper> = {
  render: () => {
    return (
      <ReactTableWrapper
        rows={mockDataForRows}
        isRowsSelectionEnabled
        onRowsSelect={action('onRowsSelect')}
        onRowDelete={action('onRowDelete')}
        tableColumns={[
          {
            name: 'AlbumId',
            dataType: 'integer',
            consoleDataType: 'integer',
            isPrimaryKey: true,
          },
          { name: 'Title', dataType: 'text', consoleDataType: 'string' },
          { name: 'ArtistId', dataType: 'integer', consoleDataType: 'integer' },
        ]}
        source={mockSource}
        table={mockTable}
        disabled={false}
        selectedRows={{}}
      />
    );
  },

  name: '🧪 Test - Open row from primary key',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // only primary key cells are links
    const primaryKeyLinks = await canvas.findAllByTestId('@open-row-AlbumId');
    await expect(primaryKeyLinks).toHaveLength(10);
    await expect(canvas.queryByTestId('@open-row-Title')).toBeNull();

    await userEvent.click(primaryKeyLinks[1]);

    // the dialog renders in a portal outside the canvas
    const dialog = within(document.body);
    await expect(await dialog.findByText('Table Row')).toBeVisible();
    await expect(dialog.getByDisplayValue('Balls to the Wall')).toBeVisible();
  },
};

export const WithRelationships: StoryObj<typeof ReactTableWrapper> = {
  render: () => {
    const relationships: Relationship[] = [
      {
        name: 'Artist',
        fromSource: 'sqlite_test',
        fromTable: ['Album'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Artist'],
          mapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
      {
        name: 'Tracks',
        fromSource: 'sqlite_test',
        fromTable: ['Album'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Track'],
          mapping: {
            AlbumId: 'AlbumId',
          },
        },
      },
    ];

    return (
      <ReactTableWrapper
        rows={mockDataForRows}
        relationships={{
          allRelationships: relationships,
          onClick: () => {
            /* noop */
          },
          onClose: () => {
            /* noop */
          },
        }}
        isRowsSelectionEnabled
        onRowsSelect={action('onRowsSelect')}
        onRowDelete={action('onRowDelete')}
        tableColumns={[]}
        source={mockSource}
        table={mockTable}
      />
    );
  },

  name: '🧪 Test - Data with Relationships',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(
      async () => {
        const albumRows = await canvas.findAllByTestId(/^@table-row-.*$/);
        await expect(albumRows.length).toBe(10);

        const firstRowOfAlbum =
          await canvas.findAllByTestId(/^@table-cell-0-.*$/);
        await expect(firstRowOfAlbum.length).toBe(5);

        await expect(firstRowOfAlbum[0]).toHaveTextContent('1'); // AlbumId
        await expect(firstRowOfAlbum[1]).toHaveTextContent(
          'For Those About To Rock We Salute You',
        );

        // The last two columns should be relationships
        await expect(firstRowOfAlbum[3]).toHaveTextContent('View');
        await expect(firstRowOfAlbum[4]).toHaveTextContent('View');
      },
      { timeout: 5000 },
    );
  },
};
