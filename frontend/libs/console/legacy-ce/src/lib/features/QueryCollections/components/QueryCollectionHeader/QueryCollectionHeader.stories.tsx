import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import { Meta } from '@storybook/react';
import {
  handlers,
  createDefaultInitialData,
} from '../../../../mocks/metadata.mock';
import { action } from '@storybook/addon-actions';

import { QueryCollectionHeader } from './QueryCollectionHeader';
import { screen, userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  title: 'Features/Query Collections/Query Collection Header',
  component: QueryCollectionHeader,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta<typeof QueryCollectionHeader>;

export const Primary = () => {
  const metadata = createDefaultInitialData();
  return (
    metadata.metadata.query_collections?.[0] && (
      <QueryCollectionHeader
        onDelete={action('delete')}
        onRename={action('rename')}
        queryCollection={metadata.metadata.query_collections?.[0]}
      />
    )
  );
};

Primary.play = async ({ canvasElement }: any) => {
  const canvas = within(canvasElement);

  // click to add operation
  await userEvent.click(await canvas.findByText('Add Operation'));

  // fill the modal box
  /* 1. click on quick add */
  await userEvent.click(await canvas.findByText('Quick Add'));

  /* 2. select introspection query */
  await waitFor(async () =>
    userEvent.click(await screen.findByText('Introspection query'))
  );

  // click on Add Operation
  await userEvent.click(
    (
      await canvas.findAllByText('Add Operation', undefined, { timeout: 3000 })
    )[1]
  );

  await userEvent.click(await canvas.findByText('Cancel'));

  // check for success notification
  // await expect(
  //   await canvas.findByText(`Successfully added operation to query collection`)
  // ).toBeInTheDocument();

  // open modal and click on add operation to see error
  await userEvent.click(await canvas.findByText('Add Operation'));
  await userEvent.click(
    (
      await canvas.findAllByText('Add Operation', undefined, { timeout: 3000 })
    )[1]
  );
  await expect(await canvas.findByText(`Name is required`)).toBeInTheDocument();
  await expect(
    await canvas.findByText(`Operation is required`)
  ).toBeInTheDocument();
  await userEvent.click(await canvas.findByText('Cancel'));

  // open modal -> click on upload operation and check for upload button
  await userEvent.click(await canvas.findByText('Add Operation'));
  await userEvent.click(await canvas.findByText('Upload Operation'));
  await expect(
    await canvas.findByText(`Upload GraphQL File`)
  ).toBeInTheDocument();
  await userEvent.click(await canvas.findByText('Cancel'));
};
