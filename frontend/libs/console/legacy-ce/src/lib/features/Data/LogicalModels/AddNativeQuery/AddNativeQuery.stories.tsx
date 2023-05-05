import { expect } from '@storybook/jest';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { screen, userEvent, within } from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { AddNativeQuery } from './AddNativeQuery';
import { handlers } from './mocks';

export default {
  component: AddNativeQuery,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ postgres: { models: true, queries: true } }, 'success'),
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

  userEvent.click(c.getByText('Add Parameter'));
  userEvent.click(c.getByText('Save'));

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
  userEvent.click(c.getByText('Remove'));

  userEvent.type(
    c.getByPlaceholderText('Name that exposes this model in GraphQL API'),
    'my_native_query'
  );
  userEvent.type(
    c.getByPlaceholderText('A description of this logical model'),
    'a description'
  );

  //select postgres from the database dropdown
  userEvent.selectOptions(
    await c.findByLabelText('Database', undefined, { timeout: 3000 }),
    await c.findByRole('option', { name: 'postgres' })
  );

  userEvent.click(c.getByText('Add Parameter'));

  userEvent.type(c.getByPlaceholderText('Parameter Name'), 'param1');
  userEvent.type(c.getByPlaceholderText('Default Value'), 'default');
  userEvent.click(c.getByTestId('required-switch'));

  userEvent.selectOptions(
    await c.findByLabelText('Query Return Type', undefined, { timeout: 3000 }),
    await c.findByRole('option', { name: 'hello_world' })
  );

  userEvent.click(c.getByText('Save'));
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
  msw: handlers(
    { postgres: { models: true, queries: true } },
    'already_exists'
  ),
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
  msw: handlers(
    { postgres: { models: true, queries: true } },
    'validation_failed'
  ),
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
  msw: handlers(
    { postgres: { models: true, queries: true } },
    'native_queries_disabled'
  ),
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
