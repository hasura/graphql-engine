import type { Meta, StoryObj } from '@storybook/react';
import { TreeComponent } from './TreeComponent';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReleaseType } from '../../../../../features/DataSource';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { getQualifiedTable } from '../../../../../features/Data/ManageTable/utils';

const meta: Meta<typeof TreeComponent> = {
  component: TreeComponent,
  decorators: [ReactQueryDecorator()],
};

export default meta;
type Story = StoryObj<typeof TreeComponent>;

const mockData = [
  {
    id: '{"dataSourceName":"chinook"}',
    dataSourceName: 'chinook',
    name: 'chinook',
    driver: 'postgres',
    releaseType: 'GA' as ReleaseType,
    children: [
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Album","schema":"public"}}',
        table: {
          name: 'Album',
          schema: 'public',
        },
        name: 'public / Album',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Artist","schema":"public"}}',
        table: {
          name: 'Artist',
          schema: 'public',
        },
        name: 'public / Artist',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Customer","schema":"public"}}',
        table: {
          name: 'Customer',
          schema: 'public',
        },
        name: 'public / Customer',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Employee","schema":"public"}}',
        table: {
          name: 'Employee',
          schema: 'public',
        },
        name: 'public / Employee',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Genre","schema":"public"}}',
        table: {
          name: 'Genre',
          schema: 'public',
        },
        name: 'public / Genre',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Invoice","schema":"public"}}',
        table: {
          name: 'Invoice',
          schema: 'public',
        },
        name: 'public / Invoice',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"InvoiceLine","schema":"public"}}',
        table: {
          name: 'InvoiceLine',
          schema: 'public',
        },
        name: 'public / InvoiceLine',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"MediaType","schema":"public"}}',
        table: {
          name: 'MediaType',
          schema: 'public',
        },
        name: 'public / MediaType',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Playlist","schema":"public"}}',
        table: {
          name: 'Playlist',
          schema: 'public',
        },
        name: 'public / Playlist',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"PlaylistTrack","schema":"public"}}',
        table: {
          name: 'PlaylistTrack',
          schema: 'public',
        },
        name: 'public / PlaylistTrack',
      },
      {
        id: '{"dataSourceName":"chinook","table":{"name":"Track","schema":"public"}}',
        table: {
          name: 'Track',
          schema: 'public',
        },
        name: 'public / Track',
      },
      {
        id: '{"dataSourceName":"chinook","function":{"name":"all_albums","schema":"public"}}',
        function: {
          name: 'all_albums',
          schema: 'public',
        },
        name: 'public / all_albums',
      },
      {
        id: '{"dataSourceName":"chinook","function":{"name":"all_albums_1","schema":"public"}}',
        function: {
          name: 'all_albums_1',
          schema: 'public',
        },
        name: 'public / all_albums_1',
      },
    ],
  },
  {
    id: '{"dataSourceName":"snowflake_test"}',
    dataSourceName: 'snowflake_test',
    name: 'snowflake_test',
    driver: 'snowflake',
    releaseType: 'GA' as ReleaseType,
    inconsistentObject: {
      definition: 'snowflake_test',
      name: 'source snowflake_test',
      reason:
        'Inconsistent object: Data connector named "snowflake" was not found in the data connector backend info',
      type: 'source',
    },
    children: [
      {
        id: '{"dataSourceName":"snowflake_test","table":["ALBUM"]}',
        table: ['ALBUM'],
        name: 'ALBUM',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["ARTIST"]}',
        table: ['ARTIST'],
        name: 'ARTIST',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["CUSTOMER"]}',
        table: ['CUSTOMER'],
        name: 'CUSTOMER',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["EMPLOYEE"]}',
        table: ['EMPLOYEE'],
        name: 'EMPLOYEE',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["GENRE"]}',
        table: ['GENRE'],
        name: 'GENRE',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["INVOICE"]}',
        table: ['INVOICE'],
        name: 'INVOICE',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["INVOICELINE"]}',
        table: ['INVOICELINE'],
        name: 'INVOICELINE',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["MEDIATYPE"]}',
        table: ['MEDIATYPE'],
        name: 'MEDIATYPE',
      },
      {
        id: '{"dataSourceName":"snowflake_test","table":["PLAYLIST"]}',
        table: ['PLAYLIST'],
        name: 'PLAYLIST',
      },
    ],
  },
];

export const Primary: Story = {
  render: () => {
    return (
      <TreeComponent
        treeData={mockData}
        onDatabaseClick={dataSourceName =>
          hasuraToast({
            title: `${dataSourceName} was clicked`,
          })
        }
        onLeafNodeClick={data => {
          const qualifiedName =
            'table' in data
              ? getQualifiedTable(data.table)
              : getQualifiedTable(data.function);
          const type = 'table' in data ? 'table' : 'function';
          hasuraToast({
            title: `${type} ${data.dataSourceName} / ${qualifiedName.join(
              ' / '
            )} was clicked`,
          });
        }}
      />
    );
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // open the database
    await expect(await canvas.getByTestId('chinook')).toBeVisible();

    // check if the number of tables inside has been rendered properly
    await expect(
      await canvas.getByTestId('chinook-object-count')
    ).toHaveTextContent('13');

    // The tree should expand on click
    await userEvent.click(await canvas.findByTestId('chinook-expand'));

    // this is a surprise tool that will help us later
    const copyOfAlbumTableReference = await canvas.getByTestId(
      'public / Album'
    );

    // check all the tables one-by-one.
    await expect(await canvas.getByTestId('public / Album')).toBeVisible();

    await expect(await canvas.getByTestId('public / Artist')).toBeVisible();
    await expect(await canvas.getByTestId('public / Customer')).toBeVisible();
    await expect(await canvas.getByTestId('public / Employee')).toBeVisible();
    await expect(await canvas.getByTestId('public / Genre')).toBeVisible();
    await expect(await canvas.getByTestId('public / Invoice')).toBeVisible();
    await expect(
      await canvas.getByTestId('public / InvoiceLine')
    ).toBeVisible();
    await expect(await canvas.getByTestId('public / MediaType')).toBeVisible();
    await expect(await canvas.getByTestId('public / Playlist')).toBeVisible();
    await expect(
      await canvas.getByTestId('public / PlaylistTrack')
    ).toBeVisible();
    await expect(await canvas.getByTestId('public / Track')).toBeVisible();
    await expect(await canvas.getByTestId('public / all_albums')).toBeVisible();
    await expect(
      await canvas.getByTestId('public / all_albums_1')
    ).toBeVisible();

    // click on a table and function and check if the callback return the right values
    await userEvent.click(await canvas.getByTestId('public / Album'));
    await expect(
      await canvas.getByText('table chinook / public / Album was clicked')
    ).toBeInTheDocument();
    await userEvent.click(await canvas.getByTestId('public / all_albums'));
    await expect(
      await canvas.getByText(
        'function chinook / public / all_albums was clicked'
      )
    ).toBeInTheDocument();

    // Check the snowflake database
    await expect(await canvas.getByTestId('snowflake_test')).toBeVisible();
    await expect(
      await canvas.getByTestId('snowflake_test-object-count')
    ).toHaveTextContent('9');

    // this one has an inconsistency. A red dot is shown on the DB icon.
    await expect(
      await canvas.getByTestId('snowflake_test-error-indicator')
    ).toBeVisible();
    await userEvent.click(await canvas.findByTestId('snowflake_test-expand'));
    await expect(await canvas.getByTestId('ALBUM')).toBeVisible();
    await expect(await canvas.getByTestId('ARTIST')).toBeVisible();
    await expect(await canvas.getByTestId('CUSTOMER')).toBeVisible();
    await expect(await canvas.getByTestId('EMPLOYEE')).toBeVisible();
    await expect(await canvas.getByTestId('GENRE')).toBeVisible();
    await expect(await canvas.getByTestId('INVOICE')).toBeVisible();
    await expect(await canvas.getByTestId('INVOICELINE')).toBeVisible();
    await expect(await canvas.getByTestId('MEDIATYPE')).toBeVisible();
    await expect(await canvas.getByTestId('PLAYLIST')).toBeVisible();

    // close one of the sources and check if the tables are visible, they shouldn't be
    await userEvent.click(await canvas.findByTestId('chinook-close'));
    await expect(copyOfAlbumTableReference).not.toBeVisible();
    await expect(await canvas.getByTestId('ALBUM')).toBeVisible();
  },
};
