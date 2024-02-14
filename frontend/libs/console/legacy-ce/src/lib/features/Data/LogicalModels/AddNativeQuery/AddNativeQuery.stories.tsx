import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import { screen, userEvent, waitFor, within } from '@storybook/testing-library';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { dismissToast } from '../../../../utils/StoryUtils';
import { NativeQuery } from '../../../hasura-metadata-types';
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

const fillAndSubmitForm: Story['play'] = async (
  { canvasElement },
  buttonText = 'Create'
) => {
  const c = within(canvasElement);

  /**
   *
   * Fill out the form and try to save:
   *
   */

  await userEvent.click(await c.findByText(buttonText));

  const errorMessages = [
    'Native Query Name is required',
    'Database is required',
    'Query Return Type is required',
  ];

  for await (const text of errorMessages) {
    await expect(await c.findByText(text)).toBeInTheDocument();
  }

  // remove param added for error testing
  //await userEvent.click(c.getAllByText('Remove')[0]);

  await userEvent.type(
    c.getByPlaceholderText('Name that exposes this model in GraphQL API'),
    'my_native_query'
  );
  await userEvent.type(
    c.getByPlaceholderText('A description of this logical model'),
    'a description'
  );

  //select postgres from the database dropdown
  await userEvent.selectOptions(
    await c.findByLabelText('Database', undefined, { timeout: 3000 }),
    await c.findByRole('option', { name: 'postgres' })
  );

  await waitForSpinnerOverlay(canvasElement);

  await userEvent.click(await c.findByText('Add Parameter'));

  await userEvent.type(c.getByPlaceholderText('Parameter Name'), 'param1');
  await userEvent.type(c.getByPlaceholderText('Description'), 'description');
  await userEvent.click(c.getByTestId('nullable-switch'));

  await userEvent.selectOptions(
    await c.findByLabelText('Query Return Type', undefined, { timeout: 3000 }),
    await c.findByRole('option', { name: 'hello_world' })
  );

  await userEvent.click(c.getByText(buttonText));
};

const defaultArgs: Story['args'] = {
  defaultSql: `SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t("one", "two")`,
};

export const Basic: Story = {
  render: args => (
    <div className="p-5">
      <AddNativeQuery {...args} />
    </div>
  ),
};

export const WithRouteWrapper: Story = {
  render: args => (
    <RouteWrapper route={Routes.CreateNativeQuery}>
      <AddNativeQuery {...args} />
    </RouteWrapper>
  ),
  name: '🚏 Route Wrapper',
  decorators: [
    ConsoleTypeDecorator({ consoleType: 'pro', menuPlacement: 'top' }),
  ],
};

export const HappyPath: Story = {
  ...Basic,
  args: {
    ...defaultArgs,
  },
  name: '😊 Happy Path',
  play: async context => {
    await fillAndSubmitForm(context);
    expect(
      await screen.findByText(
        `Successfully tracked native query as: my_native_query`,
        { exact: false },
        { timeout: 3000 }
      )
    ).toBeInTheDocument();

    await dismissToast();
  },
};

/**
 *
 * Query already exists Error
 *
 */

export const ErrorExists: Story = {
  ...HappyPath,
  name: '🚨 Already Exists',
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: true } },
      trackNativeQueryResult: 'already_exists',
    }),
  },
  play: async context => {
    await fillAndSubmitForm(context);
    expect(
      await screen.findByText(
        `Native query 'my_native_query' is already tracked.`,
        { exact: false },
        { timeout: 3000 }
      )
    ).toBeInTheDocument();

    await dismissToast();
  },
};

/**
 *
 * Validation Error
 *
 */
export const ErrorValidation: Story = {
  ...HappyPath,
  name: '🚨 Validation Error',
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: true } },
      trackNativeQueryResult: 'validation_failed',
    }),
  },
  play: async context => {
    await fillAndSubmitForm(context);
    expect(
      await screen.findByText(
        `"exec_status": "FatalError"`,
        { exact: false },
        { timeout: 3000 }
      )
    ).toBeInTheDocument();

    await dismissToast();
  },
};

/**
 *
 * Native Queries disabled
 *
 */
export const ErrorDisabled: Story = {
  ...HappyPath,
  name: '🚨 Logical Models Disabled',
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: true } },
      trackNativeQueryResult: 'native_queries_disabled',
    }),
  },
  play: async context => {
    await fillAndSubmitForm(context);
    expect(
      await screen.findByText(
        `NativeQueries is disabled!`,
        { exact: false },
        { timeout: 3000 }
      )
    ).toBeInTheDocument();

    await dismissToast();
  },
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
      q.root_field_name
    );
    await expect(await c.findByTestId('comment')).toHaveValue(q.comment);

    // make sure this element is findable before we try to wait for the value via getBy
    await c.findByTestId('returns');

    // we need this bc the value takes a second to get set
    await waitFor(() =>
      expect(c.getByTestId('returns')).toHaveValue(q.returns)
    );

    // wait for the value to be what we expect NOT wait for the element
    await waitFor(() =>
      expect(c.getByTestId('arguments.0.name')).toHaveValue(firstArgumentName)
    );

    // same as above:
    await waitFor(() =>
      expect(c.getByTestId('arguments.0.type')).toHaveValue(
        q.arguments.query.type
      )
    );

    await expect(await c.findByTestId('arguments.0.description')).toHaveValue(
      q.arguments.query.description
    );
    await expect(await c.findByTestId('nullable-switch')).toHaveAttribute(
      'data-state',
      'unchecked'
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
        'Looking to add Native Queries for SQL Server/Big Query databases?'
      )
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
        'Looking to add Native Queries for SQL Server/Big Query databases?'
      )
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
      }
    );
  },
};
