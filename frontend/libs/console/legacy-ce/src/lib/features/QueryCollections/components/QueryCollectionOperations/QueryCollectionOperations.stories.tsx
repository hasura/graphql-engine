import {
  expect,
  screen,
  userEvent,
  within,
  waitForElementToBeRemoved,
} from 'storybook/test';
import { Meta, StoryObj } from '@storybook/react-webpack5';
import { handlers, ReactQueryDecorator } from '@hasura/shared/testing';

import { QueryCollectionsOperations } from './QueryCollectionOperations';

export default {
  title: 'Features/Query Collections/Query Collections Operations',
  component: QueryCollectionsOperations,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta<typeof QueryCollectionsOperations>;

export const Primary: StoryObj = {
  render: () => {
    return <QueryCollectionsOperations collectionName="allowed-queries" />;
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // wait for the query collection to load
    await expect(
      canvas.queryByTestId('query-collection-operations-loading'),
    ).toBeInTheDocument();

    await waitForElementToBeRemoved(
      () => screen.queryByTestId('query-collection-operations-loading'),
      { timeout: 2000 },
    );

    //
    await expect(canvas.queryByTestId('operation-MyQuery')).toBeInTheDocument();

    // check that the controls are not visible by default
    await expect(
      await canvas.queryByTestId('selected-operations-controls'),
    ).not.toBeInTheDocument();

    // select the first operation
    await userEvent.click(canvas.getByTestId('operation-MyQuery'));
    await userEvent.click(canvas.getByTestId('operation-MyQuery'));

    // check that the controls are visible
    await canvas.queryByTestId('selected-operations-controls');
    await expect(
      await canvas.queryByTestId('selected-operations-controls'),
    ).toBeInTheDocument();

    // click the move feature
    await userEvent.click(canvas.getByText('Move'));
    // the submenu should be visible
    await expect(screen.queryByText('other_queries')).toBeInTheDocument();
    // click the submenu item
    await userEvent.click(screen.getByText('other_queries'));
    // the submenu should be hidden
    await expect(screen.queryByText('other_queries')).not.toBeInTheDocument();

    // click the copy feature
    await userEvent.click(canvas.getByText('Copy'));
    // the submenu should be visible
    await expect(screen.queryByText('other_queries')).toBeInTheDocument();
    // click the submenu item
    await userEvent.click(screen.getByText('other_queries'));
    // the submenu should be hidden
    await expect(screen.queryByText('other_queries')).not.toBeInTheDocument();

    // test the search feature
    await userEvent.type(canvas.getByTestId('search'), 'query2');
    // the right rows should be visible
    await expect(
      canvas.queryByTestId('operation-MyQuery3'),
    ).not.toBeInTheDocument();
    await expect(
      canvas.queryByTestId('operation-MyQuery2'),
    ).toBeInTheDocument();
    // clear the search
    await userEvent.clear(canvas.getByTestId('search'));
    // all the rows should be visible
    await expect(
      canvas.queryByTestId('operation-MyQuery2'),
    ).toBeInTheDocument();

    // click the select all feature
    await userEvent.click(canvas.getByTestId('query-collections-select-all'));
    // all the rows should be selected
    await expect(
      await canvas.queryByTestId('selected-operations-controls'),
    ).toBeInTheDocument();

    // click again the select all feature
    await userEvent.click(canvas.getByTestId('query-collections-select-all'));
    // all the rows should be deselected
    await expect(
      await canvas.queryByTestId('selected-operations-controls'),
    ).not.toBeInTheDocument();
  },
};
