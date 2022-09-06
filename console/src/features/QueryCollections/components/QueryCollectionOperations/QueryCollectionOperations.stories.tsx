import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, Story } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { within, userEvent, screen } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { waitForElementToBeRemoved } from '@testing-library/react';

import { QueryCollectionsOperations } from './QueryCollectionOperations';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Features/Query Collections/Query Collections Operations',
  component: QueryCollectionsOperations,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof QueryCollectionsOperations>;

export const Primary: Story = () => {
  return (
    <QueryCollectionsOperations
      onEdit={action('onEdit')}
      collectionName="allowed-queries"
    />
  );
};

Primary.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // wait for the query collection to load
  expect(
    canvas.queryByTestId('query-collection-operations-loading')
  ).toBeInTheDocument();

  await waitForElementToBeRemoved(
    () => screen.queryByTestId('query-collection-operations-loading'),
    { timeout: 2000 }
  );

  //
  expect(canvas.queryByTestId('operation-MyQuery')).toBeInTheDocument();

  // check that the controls are not visible by default
  expect(
    await canvas.queryByTestId('selected-operations-controls')
  ).not.toBeInTheDocument();

  // select the first operation
  await userEvent.click(canvas.getByTestId('operation-MyQuery'));

  // check that the controls are visible
  expect(
    await canvas.queryByTestId('selected-operations-controls')
  ).toBeInTheDocument();

  // click the move feature
  await userEvent.click(canvas.getByText('Move'));
  // the submenu should be visible
  expect(screen.queryByText('other-queries')).toBeInTheDocument();
  // click the submenu item
  await userEvent.click(screen.getByText('other-queries'));
  // the submenu should be hidden
  expect(screen.queryByText('other-queries')).not.toBeInTheDocument();

  // click the copy feature
  await userEvent.click(canvas.getByText('Copy'));
  // the submenu should be visible
  expect(screen.queryByText('other-queries')).toBeInTheDocument();
  // click the submenu item
  await userEvent.click(screen.getByText('other-queries'));
  // the submenu should be hidden
  expect(screen.queryByText('other-queries')).not.toBeInTheDocument();

  // test the search feature
  await userEvent.type(canvas.getByTestId('search'), 'mutation');
  // the right rows should be visible
  expect(canvas.queryByTestId('operation-MyQuery')).not.toBeInTheDocument();
  expect(canvas.queryByTestId('operation-MyMutation')).toBeInTheDocument();
  // clear the search
  await userEvent.clear(canvas.getByTestId('search'));
  // all the rows should be visible
  expect(canvas.queryByTestId('operation-MyQuery')).toBeInTheDocument();

  // click the select all feature
  await userEvent.click(canvas.getByTestId('query-collections-select-all'));
  // all the rows should be selected
  expect(
    await canvas.queryByTestId('selected-operations-controls')
  ).toBeInTheDocument();

  // click again the select all feature
  await userEvent.click(canvas.getByTestId('query-collections-select-all'));
  // all the rows should be deselected
  expect(
    await canvas.queryByTestId('selected-operations-controls')
  ).not.toBeInTheDocument();
};
