import { expect } from '@storybook/jest';
import { StoryObj } from '@storybook/react';
import { screen, userEvent, within } from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { dismissToast } from '../../../../utils/StoryUtils';
import { AddNativeQuery } from './AddNativeQuery';
import { nativeQueryHandlers } from './mocks';
import { RouteWrapper } from '../components/RouteWrapper';

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
};

const fillAndSubmitForm: Story['play'] = async ({ canvasElement }) => {
  const c = within(canvasElement);

  /**
   *
   * Fill out the form and try to save:
   *
   */

  await userEvent.click(c.getByText('Add Parameter'));
  await userEvent.click(c.getByText('Save'));

  const errorMessages = [
    'Native Query Name is required',
    'Database is required',
    'Paramater Name is required',
    'Query Return Type is required',
  ];

  for await (const text of errorMessages) {
    await expect(await c.findByText(text)).toBeInTheDocument();
  }

  // remove param added for error testing
  await userEvent.click(c.getByText('Remove'));

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

  await userEvent.click(await c.findByText('Add Parameter'));

  await userEvent.type(c.getByPlaceholderText('Parameter Name'), 'param1');
  await userEvent.type(c.getByPlaceholderText('Default Value'), 'default');
  await userEvent.click(c.getByTestId('required-switch'));

  await userEvent.selectOptions(
    await c.findByLabelText('Query Return Type', undefined, { timeout: 3000 }),
    await c.findByRole('option', { name: 'hello_world' })
  );

  await userEvent.click(c.getByText('Save'));
};

const defaultArgs: Story['args'] = {
  defaultFormValues: {
    code: `SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t("one", "two")`,
  },
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
    <RouteWrapper pathname={'/data/native-queries/create'}>
      <AddNativeQuery {...args} />
    </RouteWrapper>
  ),
  name: '🚏 Route Wrapper',
  parameters: {
    consoleType: 'pro',
  },
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
