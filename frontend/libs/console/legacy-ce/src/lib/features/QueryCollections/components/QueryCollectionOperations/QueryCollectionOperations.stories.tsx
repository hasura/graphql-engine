import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import {
  screen,
  userEvent,
  waitForElementToBeRemoved,
  within,
} from '@storybook/testing-library';
import { handlers } from '../../../../mocks/metadata.mock';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';

import { QueryCollectionsOperations } from './QueryCollectionOperations';

export default {
  title: 'Features/Query Collections/Query Collections Operations',
  component: QueryCollectionsOperations,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
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
    await userEvent.click(canvas.getByTestId('operation-MyQuery'));

    // check that the controls are visible
    await canvas.queryByTestId('selected-operations-controls');
    expect(
      await canvas.queryByTestId('selected-operations-controls')
    ).toBeInTheDocument();

    // click the move feature
    await userEvent.click(canvas.getByText('Move'));
    // the submenu should be visible
    expect(screen.queryByText('other_queries')).toBeInTheDocument();
    // click the submenu item
    await userEvent.click(screen.getByText('other_queries'));
    // the submenu should be hidden
    expect(screen.queryByText('other_queries')).not.toBeInTheDocument();

    // click the copy feature
    await userEvent.click(canvas.getByText('Copy'));
    // the submenu should be visible
    expect(screen.queryByText('other_queries')).toBeInTheDocument();
    // click the submenu item
    await userEvent.click(screen.getByText('other_queries'));
    // the submenu should be hidden
    expect(screen.queryByText('other_queries')).not.toBeInTheDocument();

    // test the search feature
    await userEvent.type(canvas.getByTestId('search'), 'query2');
    // the right rows should be visible
    expect(canvas.queryByTestId('operation-MyQuery3')).not.toBeInTheDocument();
    expect(canvas.queryByTestId('operation-MyQuery2')).toBeInTheDocument();
    // clear the search
    await userEvent.clear(canvas.getByTestId('search'));
    // all the rows should be visible
    expect(canvas.queryByTestId('operation-MyQuery2')).toBeInTheDocument();

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
  },
};
