import { ReactQueryDecorator } from '@hasura/shared/testing';
import { StoryObj, StoryFn, Meta } from '@storybook/react-webpack5';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { action } from 'storybook/actions';
import { BrowseRows } from './BrowseRows';
import { handlers } from './__mocks__/handlers.mock';

export default {
  component: BrowseRows,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers('http://localhost:8080'),
  },
} as Meta<typeof BrowseRows>;

export const Basic: StoryFn<typeof BrowseRows> = () => {
  return (
    <BrowseRows
      table={['Album']}
      dataSourceName="sqlite_test"
      primaryKeys={[]}
      onUpdateOptions={action('onUpdateOptions')}
    />
  );
};

export const BasicDisplayTest: StoryObj<typeof BrowseRows> = {
  render: () => {
    return (
      <BrowseRows
        table={['Album']}
        dataSourceName="sqlite_test"
        primaryKeys={[]}
        onUpdateOptions={action('onUpdateOptions')}
      />
    );
  },

  name: '🧪 Test - Table with Relationships',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    /**
     * There should be tab-button that displays the current table name
     */
    const albumTab = await canvas.findByTestId('@tab-Album');

    await expect(albumTab).toHaveTextContent('Album');

    /**
     * The current table should have 10 rows by default
     */
    await waitFor(
      async () => {
        const albumRows = await canvas.findAllByTestId(/^@table-row-.*$/);
        await expect(albumRows.length).toBe(10);
      },
      { timeout: 10000 },
    );

    /**
     * The current table should have 10 rows by default
     */
    let firstRowOfAlbum = await canvas.findAllByTestId(/^@table-cell-0-.*$/);

    /**
     * Each row should have 5 columns - Id, Title, ArtistId, Tracks, Artist. So we'll sample the first row to check that
     */
    await expect(firstRowOfAlbum.length).toBe(5);

    /**
     * Each row should have 5 columns - Id, Title, ArtistId, Tracks, Artist. So we'll sample the first row to check that
     * and we'll also check the row values
     */
    await expect(firstRowOfAlbum[0]).toHaveTextContent('1');
    await expect(firstRowOfAlbum[1]).toHaveTextContent(
      'For Those About To Rock We Salute You',
    );
    await expect(firstRowOfAlbum[2]).toHaveTextContent('1');
    await expect(firstRowOfAlbum[3]).toHaveTextContent('View'); // This is a relationship
    await expect(firstRowOfAlbum[4]).toHaveTextContent('View'); // This is a relationship

    /**
     * Click on the relationship of Artist
     */
    const relationshipViewLink = await canvas.findAllByTestId(
      '@view-relationship-Artist',
    );
    await userEvent.click(relationshipViewLink[0]);

    /**
     * Check if the artist tab has been opened
     */
    const artistTab = await canvas.findByTestId('@tab-Album.Artist');
    await expect(artistTab).toHaveTextContent('Album.Artist');

    /**
     * Validate that we can see the 1 row of Artist
     */
    await waitFor(
      async () => {
        const artistRows = await canvas.findAllByTestId(/^@table-row-.*$/);
        await expect(artistRows.length).toBe(1);
      },
      { timeout: 5000 },
    );

    const firstRowOfArtist = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
    await expect(firstRowOfArtist.length).toBe(2);

    /**
     * Validate that the row values are correct
     */
    await expect(firstRowOfArtist[0]).toHaveTextContent('1'); // ArtistId
    await expect(firstRowOfArtist[1]).toHaveTextContent('AC/DC');

    // Go back to Album
    await userEvent.click(albumTab);

    // only one row should there
    await waitFor(
      async () => {
        const albumRows = await canvas.findAllByTestId(/^@table-row-.*$/);
        await expect(albumRows.length).toBe(1);
      },
      { timeout: 5000 },
    );

    // No change to columns length
    firstRowOfAlbum = await canvas.findAllByTestId(/^@table-cell-0-.*$/);
    await expect(firstRowOfAlbum.length).toBe(5);

    /**
     * The go-to Artist relationship view link should be visible
     */
    await expect(
      await canvas.findByTestId('@view-relationship-Artist-goto-link'),
    ).toBeVisible();

    /**
     * close the relationship view
     */
    const artistTabCloseBtn = await canvas.findByTestId(
      '@tab-Album.Artist-close',
    );
    await userEvent.click(artistTabCloseBtn);

    /**
     * Once Album.Artist tab is closed, the Album table should go back to being 10 rows with 10 links for Album.Artist
     */
    await expect(
      (await canvas.findAllByTestId('@view-relationship-Artist')).length,
    ).toBe(10);
  },
};
