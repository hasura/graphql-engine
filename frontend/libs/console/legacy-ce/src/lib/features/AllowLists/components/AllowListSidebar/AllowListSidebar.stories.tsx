import { ReactQueryDecorator, handlers } from '@hasura/shared/testing';
import { Meta } from '@storybook/react-webpack5';
import { action } from 'storybook/actions';

import { AllowListSidebar } from './AllowListSidebar';
import { expect, screen, userEvent, waitFor, within } from 'storybook/test';

export default {
  title: 'Features/Allow List/Allow List Sidebar',
  component: AllowListSidebar,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta<typeof AllowListSidebar>;

export const Primary = () => (
  <AllowListSidebar
    buildQueryCollectionHref={() => '#'}
    onQueryCollectionClick={action('onQueryCollectionClick')}
    onQueryCollectionCreate={action('onQueryCollectionCreate')}
    selectedCollectionQuery="allowed-queries"
  />
);

Primary.play = async ({ canvasElement }: any) => {
  const canvas = within(canvasElement);

  // type collection name
  await userEvent.type(
    canvas.getByPlaceholderText('Search Collections...'),
    'allowed',
  );

  // expect to have only allowed-queried in collcetion
  await expect(await canvas.findByText(`allowed-queries`)).toBeInTheDocument();
  await waitFor(async () => {
    await expect(screen.queryByText('other_queries')).not.toBeInTheDocument();
  });
  // type queries
  await userEvent.clear(canvas.getByPlaceholderText('Search Collections...'));

  await userEvent.type(
    canvas.getByPlaceholderText('Search Collections...'),
    'queries',
  );

  // expect to have both collection
  await expect(await canvas.findByText(`allowed-queries`)).toBeInTheDocument();
  await expect(await canvas.findByText(`other_queries`)).toBeInTheDocument();
};
