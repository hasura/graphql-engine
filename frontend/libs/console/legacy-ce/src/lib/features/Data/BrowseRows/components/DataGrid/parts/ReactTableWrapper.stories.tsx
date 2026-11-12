import { ReactQueryDecorator } from '@hasura/shared/testing';
import { StoryObj, StoryFn, Meta } from '@storybook/react-webpack5';
import { Relationship } from '../../../../../DatabaseRelationships';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { action } from 'storybook/actions';
import { TableColumn, TableRow } from '@hasura/metadata/data-source';
import { ReactTableWrapper } from './ReactTableWrapper';
import { MAX_CELL_TEXT_LENGTH } from '../utils';
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

// Big data scenarios. Cells truncate values longer than MAX_CELL_TEXT_LENGTH
// so large values don't freeze the page; the row dialog shows them in full.

const largeByteaRows = Array.from({ length: 50 }, (_, index) => ({
  id: index + 1,
  name: `file_${index + 1}.bin`,
  // 200,000 bytes per row as hex, like a bytea column holding a file
  content: `\\x${(index % 16).toString(16).repeat(400_000)}`,
}));

const largeByteaColumns: TableColumn[] = [
  {
    name: 'id',
    dataType: 'integer',
    consoleDataType: 'integer',
    isPrimaryKey: true,
  },
  { name: 'content', dataType: 'bytea', consoleDataType: 'binary' },
  { name: 'name', dataType: 'text', consoleDataType: 'string' },
];

export const LargeByteaValues: StoryObj<typeof ReactTableWrapper> = {
  render: () => (
    <ReactTableWrapper
      rows={largeByteaRows}
      isRowsSelectionEnabled
      onRowsSelect={action('onRowsSelect')}
      onRowDelete={action('onRowDelete')}
      tableColumns={largeByteaColumns}
      source={mockSource}
      table={['files']}
      disabled={false}
      selectedRows={{}}
    />
  ),

  name: '🧪 Test - Large bytea values are offered as downloads',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findAllByTestId(/^@table-row-.*$/)).toHaveLength(
      50,
    );

    // columns: id (primary key), content, name
    await expect(canvas.getByTestId('@table-cell-0-2')).toHaveTextContent(
      'Download (195.3 KB)',
    );
    await expect(canvas.getAllByTestId('@download-content')).toHaveLength(50);
    await expect(canvas.getByTestId('@table-cell-0-3')).toHaveTextContent(
      'file_1.bin',
    );

    // binary values have no meaningful order
    await expect(canvas.getByText('content').parentElement).not.toHaveClass(
      'cursor-pointer',
    );
    await expect(canvas.getByText('name').parentElement).toHaveClass(
      'cursor-pointer',
    );
  },
};

const largeJsonbRows = Array.from({ length: 50 }, (_, index) => ({
  id: index + 1,
  // ~40 KB of JSON per row, like a jsonb document
  payload: {
    events: Array.from({ length: 1000 }, (__, event) => ({
      event,
      type: event % 2 ? 'click' : 'view',
    })),
  },
  // TableRow only declares primitives, but jsonb values are objects at runtime
})) as unknown as TableRow[];

export const LargeJsonbValues: StoryObj<typeof ReactTableWrapper> = {
  render: () => (
    <ReactTableWrapper
      rows={largeJsonbRows}
      isRowsSelectionEnabled
      onRowsSelect={action('onRowsSelect')}
      onRowDelete={action('onRowDelete')}
      tableColumns={[
        {
          name: 'id',
          dataType: 'integer',
          consoleDataType: 'integer',
          isPrimaryKey: true,
        },
        { name: 'payload', dataType: 'jsonb', consoleDataType: 'json' },
      ]}
      source={mockSource}
      table={['events']}
      disabled={false}
      selectedRows={{}}
    />
  ),

  name: '🧪 Test - Large jsonb values are truncated',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findAllByTestId(/^@table-row-.*$/)).toHaveLength(
      50,
    );

    const payload = canvas.getByTestId('@table-cell-0-2');
    await expect(payload.textContent?.length).toBeLessThan(
      MAX_CELL_TEXT_LENGTH + 50,
    );
    await expect(payload.textContent).toMatch(
      /^\{"events":\[\{"event":0,"type":"view"\}.*… \([\d,]+ characters\)$/,
    );
  },
};

const longPrimaryKey = `key-${'x'.repeat(MAX_CELL_TEXT_LENGTH * 2)}`;
const longValue = 'lorem ipsum '.repeat(500);

export const LongPrimaryKeyIsNotTruncated: StoryObj<typeof ReactTableWrapper> =
  {
    render: () => (
      <ReactTableWrapper
        rows={[{ key: longPrimaryKey, value: longValue }]}
        isRowsSelectionEnabled
        onRowsSelect={action('onRowsSelect')}
        onRowDelete={action('onRowDelete')}
        tableColumns={[
          {
            name: 'key',
            dataType: 'text',
            consoleDataType: 'string',
            isPrimaryKey: true,
          },
          { name: 'value', dataType: 'text', consoleDataType: 'string' },
        ]}
        source={mockSource}
        table={['documents']}
        disabled={false}
        selectedRows={{}}
      />
    ),

    name: '🧪 Test - Long primary key is kept whole, full value in row dialog',

    play: async ({ canvasElement }) => {
      const canvas = within(canvasElement);

      // primary keys identify the row, so they are never truncated
      const primaryKey = await canvas.findByTestId('@open-row-key');
      await expect(primaryKey.textContent).toBe(longPrimaryKey);

      const value = canvas.getByTestId('@table-cell-0-2');
      await expect(value).toHaveTextContent('characters)');

      // the row dialog shows the full value
      await userEvent.click(primaryKey);
      const dialog = within(document.body);
      await expect(await dialog.findByText('Table Row')).toBeVisible();
      await expect(
        dialog.getByText((_, element) => element?.textContent === longValue),
      ).toBeInTheDocument();
    },
  };

const manyColumnNames = Array.from(
  { length: 20 },
  (_, index) => `column_${String(index + 1).padStart(2, '0')}`,
);

const manyRows = Array.from({ length: 1000 }, (_, row) => ({
  id: row + 1,
  ...Object.fromEntries(
    manyColumnNames.map((name, column) => [name, `r${row + 1}c${column + 1}`]),
  ),
}));

export const ManyRowsAndColumns: StoryObj<typeof ReactTableWrapper> = {
  render: () => (
    <ReactTableWrapper
      rows={manyRows}
      isRowsSelectionEnabled
      onRowsSelect={action('onRowsSelect')}
      onRowDelete={action('onRowDelete')}
      tableColumns={[
        {
          name: 'id',
          dataType: 'integer',
          consoleDataType: 'integer',
          isPrimaryKey: true,
        },
        ...manyColumnNames.map((name): TableColumn => ({
          name,
          dataType: 'text',
          consoleDataType: 'string',
        })),
      ]}
      source={mockSource}
      table={['wide_table']}
      disabled={false}
      selectedRows={{}}
    />
  ),

  name: '🧪 Test - 1,000 rows by 21 columns',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findAllByTestId(/^@table-row-.*$/)).toHaveLength(
      1000,
    );
    await expect(canvas.getByTestId('@table-cell-999-1')).toHaveTextContent(
      '1000',
    );
    await expect(canvas.getByTestId('@table-cell-999-21')).toHaveTextContent(
      'r1000c20',
    );
  },
};
