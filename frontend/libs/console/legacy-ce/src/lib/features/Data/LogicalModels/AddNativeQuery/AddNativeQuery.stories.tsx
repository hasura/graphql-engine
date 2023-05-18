import { expect } from '@storybook/jest';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { screen, userEvent, within } from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { AddNativeQuery } from './AddNativeQuery';
import { nativeQueryHandlers } from './mocks';

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
} as ComponentMeta<typeof AddNativeQuery>;

export const Basic: ComponentStory<typeof AddNativeQuery> = args => {
  return <AddNativeQuery />;
};

const fillAndSubmitForm = async ({
  canvasElement,
}: {
  canvasElement: HTMLElement;
}) => {
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

export const HappyPath: ComponentStory<typeof AddNativeQuery> = args => {
  return (
    <AddNativeQuery
      defaultFormValues={{
        code: `SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t("one", "two")`,
      }}
    />
  );
};

HappyPath.storyName = '😊 Happy Path';

HappyPath.play = async ({ canvasElement }) => {
  fillAndSubmitForm({ canvasElement });
  expect(
    await screen.findByText(
      `Successfully tracked native query as: my_native_query`,
      { exact: false },
      { timeout: 3000 }
    )
  ).toBeInTheDocument();
};

/**
 *
 * Query already exists Error
 *
 */
export const ErrorExists: ComponentStory<typeof AddNativeQuery> = args => {
  return (
    <AddNativeQuery
      defaultFormValues={{
        code: `SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t("one", "two")`,
      }}
    />
  );
};
ErrorExists.storyName = '🚨 Already Exists';
ErrorExists.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: { postgres: { models: true, queries: true } },
    trackNativeQueryResult: 'already_exists',
  }),
};

ErrorExists.play = async ({ canvasElement }) => {
  fillAndSubmitForm({ canvasElement });
  expect(
    await screen.findByText(
      `Native query 'my_native_query' is already tracked.`,
      { exact: false },
      { timeout: 3000 }
    )
  ).toBeInTheDocument();
};

/**
 *
 * Validation Error
 *
 */
export const ErrorValidation: ComponentStory<typeof AddNativeQuery> = args => {
  return (
    <AddNativeQuery
      defaultFormValues={{
        code: `select * from foo`,
      }}
    />
  );
};
ErrorValidation.storyName = '🚨 Validation Error';
ErrorValidation.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: { postgres: { models: true, queries: true } },
    trackNativeQueryResult: 'validation_failed',
  }),
};

ErrorValidation.play = async ({ canvasElement }) => {
  fillAndSubmitForm({ canvasElement });
  expect(
    await screen.findByText(
      `"exec_status": "FatalError"`,
      { exact: false },
      { timeout: 3000 }
    )
  ).toBeInTheDocument();
};

/**
 *
 * Native Queries disabled
 *
 */
export const ErrorDisabled: ComponentStory<typeof AddNativeQuery> = args => {
  return (
    <AddNativeQuery
      defaultFormValues={{
        code: `SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t("one", "two")`,
      }}
    />
  );
};
ErrorDisabled.storyName = '🚨 Logical Models Disabled';
ErrorDisabled.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: { postgres: { models: true, queries: true } },
    trackNativeQueryResult: 'native_queries_disabled',
  }),
};

ErrorDisabled.play = async ({ canvasElement }) => {
  fillAndSubmitForm({ canvasElement });
  expect(
    await screen.findByText(
      `NativeQueries is disabled!`,
      { exact: false },
      { timeout: 3000 }
    )
  ).toBeInTheDocument();
};
