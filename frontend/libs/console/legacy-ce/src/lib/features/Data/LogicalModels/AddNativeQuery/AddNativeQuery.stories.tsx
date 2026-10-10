import { expect, waitFor, within } from 'storybook/test';
import { Meta, StoryObj } from '@storybook/react-webpack5';
import {
  ConsoleTypeDecorator,
  ReactQueryDecorator,
} from '@hasura/shared/testing';
import { NativeQuery } from '@hasura/shared/types';
import { waitForSpinnerOverlay } from '../../components/ReactQueryWrappers/story-utils';
import { RouteWrapper } from '../components/RouteWrapper';
import { Routes } from '../constants';
import { AddNativeQuery } from './AddNativeQuery';
import { nativeQueryHandlers } from './mocks';
import { eeLicenseInfo } from '../../../EETrial/mocks/http';

type Story = StoryObj<typeof AddNativeQuery>;

export default {
  component: AddNativeQuery,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: true } },
      trackNativeQueryResult: 'success',
    }),
    layout: 'fullscreen',
  },
} satisfies Meta<typeof AddNativeQuery>;

export const Basic: Story = {
  render: (args) => (
    <div className="p-5">
      <AddNativeQuery {...args} />
    </div>
  ),
};

export const WithRouteWrapper: Story = {
  render: (args) => (
    <RouteWrapper route={Routes.CreateNativeQuery}>
      <AddNativeQuery {...args} />
    </RouteWrapper>
  ),
  name: '🚏 Route Wrapper',
  decorators: [
    ConsoleTypeDecorator({ consoleType: 'pro', menuPlacement: 'top' }),
  ],
};

const existingNativeQuery: Required<NativeQuery> = {
  arguments: {
    query: {
      nullable: false,
      type: 'varchar',
      description: 'a nice description',
    },
  },
  type: 'query',
  comment: 'a very nice comment',
  code: "select\n    AlbumId,\n    a.ArtistId,\n    Title [AlbumTitle],\n    a.Name [Artist],\n    (\n      select\n        count(*)\n      from\n        Track t\n      where\n        t.AlbumId = b.AlbumId\n    ) [TrackCount]\n  from\n    album b\n    join artist a on a.ArtistId = b.ArtistId\n    \n    \n -- search option for later:   \n  WHERE\n    b.Title like '%' + {{query}} + '%'",
  returns: 'hello_world',
  root_field_name: 'AlbumDetail',
  object_relationships: [],
  array_relationships: [],
};

// This story validates that the above Native Query's properties are rendered in the UI correctly for edit/update situations:
export const Update: Story = {
  ...Basic,
  name: '💾 Update/view existing',
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: true } },
    }),
  },
  args: {
    editDetails: {
      nativeQuery: existingNativeQuery,
      dataSourceName: 'postgres',
    },
  },

  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    const q = existingNativeQuery;
    const firstArgumentName = Object.keys(q.arguments)[0];

    await expect(await c.findByTestId('root_field_name')).toHaveValue(
      q.root_field_name,
    );
    await expect(await c.findByTestId('comment')).toHaveValue(q.comment);

    // make sure this element is findable before we try to wait for the value via getBy
    await c.findByTestId('returns');

    // we need this bc the value takes a second to get set
    await waitFor(() =>
      expect(c.getByTestId('returns')).toHaveValue(q.returns),
    );

    // wait for the value to be what we expect NOT wait for the element
    await waitFor(() =>
      expect(c.getByTestId('arguments.0.name')).toHaveValue(firstArgumentName),
    );

    // same as above:
    await waitFor(() =>
      expect(c.getByTestId('arguments.0.type')).toHaveValue(
        q.arguments.query.type,
      ),
    );

    await expect(await c.findByTestId('arguments.0.description')).toHaveValue(
      q.arguments.query.description,
    );
    await expect(await c.findByTestId('nullable-switch')).toHaveAttribute(
      'data-state',
      'unchecked',
    );
  },
};

export const EEBannerDisplayed: typeof Basic = {
  ...Basic,
  name: '⭐️ EE License - None',
  decorators: [
    ConsoleTypeDecorator({ consoleType: 'pro-lite', menuPlacement: 'top' }),
  ],
  parameters: {
    msw: [
      eeLicenseInfo.none,
      ...nativeQueryHandlers({
        metadataOptions: { postgres: { models: true, queries: true } },
        trackNativeQueryResult: 'native_queries_disabled',
      }),
    ],
  },
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await waitForSpinnerOverlay(canvasElement);

    await expect(
      await c.findByText(
        'Looking to add Native Queries for SQL Server/Big Query databases?',
      ),
    ).toBeInTheDocument();

    await expect(c.getByText('Enable Enterprise')).toBeInTheDocument();

    //checks that the correct options are present in the dropdown
    await expect(c.queryByTestId('source-mssql')).not.toBeInTheDocument();
    await expect(c.getByTestId('source-postgres')).toBeInTheDocument();
  },
};

export const EEBannerNotDisplayed: typeof Basic = {
  ...Basic,
  name: '⭐️ EE License - Active',
  decorators: [
    ConsoleTypeDecorator({ consoleType: 'pro-lite', menuPlacement: 'top' }),
  ],
  parameters: {
    msw: [
      eeLicenseInfo.active,
      ...nativeQueryHandlers({
        metadataOptions: { postgres: { models: true, queries: true } },
        trackNativeQueryResult: 'native_queries_disabled',
      }),
    ],
  },
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await waitForSpinnerOverlay(canvasElement);
    await expect(
      await c.queryByText(
        'Looking to add Native Queries for SQL Server/Big Query databases?',
      ),
    ).not.toBeInTheDocument();

    await expect(c.queryByText('Enable Enterprise')).not.toBeInTheDocument();

    //checks that the correct options are present in the dropdown
    await waitFor(
      async () => {
        await expect(c.getByTestId('source-mssql')).toBeInTheDocument();
        await expect(c.getByTestId('source-postgres')).toBeInTheDocument();
      },
      {
        timeout: 5000,
      },
    );
  },
};
