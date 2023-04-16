import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import {
  userEvent,
  waitFor,
  waitForElementToBeRemoved,
  within,
} from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { expect } from '@storybook/jest';
import { dangerouslyDelay } from '../../../../storybook/utils/dangerouslyDelay';

import { handlers, resetMetadata } from './handlers.mock';
import { ManageTrackedTables } from '../../ManageTable/components/ManageTrackedTables';

export default {
  title: 'Data/Components/ManageTrackedTables',
  component: ManageTrackedTables,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ManageTrackedTables>;

export const UntrackedTables: ComponentStory<
  typeof ManageTrackedTables
> = () => <ManageTrackedTables dataSourceName="chinook" />;

UntrackedTables.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Reset initial metadata to make sure tests start from a clean slate everytime
  resetMetadata();

  // Wait until it finishes loading
  await waitFor(() => canvas.findByTestId('track-tables'), {
    timeout: 5000,
  });

  // Verify it correctly displays untracked tables by default
  await expect(canvas.getByText('public / Invoice')).toBeInTheDocument();
  await expect(canvas.getByText('public / InvoiceLine')).toBeInTheDocument();
  await expect(canvas.getByText('public / Track')).toBeInTheDocument();
  await expect(canvas.getByText('public / Playlist')).toBeInTheDocument();
  await expect(canvas.getByText('public / PlaylistTrack')).toBeInTheDocument();
  await expect(canvas.getByText('public / MediaType')).toBeInTheDocument();
};

export const Untrack: ComponentStory<typeof ManageTrackedTables> = () => (
  <ManageTrackedTables dataSourceName="chinook" />
);

Untrack.storyName = 'Test - Untrack a table 🧪';

Untrack.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Reset initial metadata to make sure tests start from a clean slate everytime
  resetMetadata();

  // Wait until it finishes loading
  await waitFor(() => canvas.findByTestId('track-tables'), {
    timeout: 5000,
  });

  userEvent.click(await canvas.findByText('Tracked'));

  // Wait for the button to appear on the screen using findBy. Store it in a variable to click it afterwards.
  const button = await canvas.findByTestId(`untrack-public.Artist`);

  // Wait an additional second, otherwise clicking does not fire the request
  // Tried to figure out how to avoid using delay and favor waitFor and await findBy,
  // but could not find a visual cue that indicates that clicking the button will work
  // This might indicate that the button must wait for some asynchronous operation before it's ready
  await dangerouslyDelay(1000);

  // Track public.Invoice
  userEvent.click(button);

  // It should not be in the Tracked tab anymore
  await waitForElementToBeRemoved(() => canvas.queryByText('public / Artist'), {
    timeout: 2000,
  });
};

export const Track: ComponentStory<typeof ManageTrackedTables> = () => (
  <ManageTrackedTables dataSourceName="chinook" />
);

Track.storyName = 'Test - Track a table 🧪';

Track.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Reset initial metadata to make sure tests start from a clean slate everytime
  resetMetadata();

  // Wait until it finishes loading
  await waitFor(() => canvas.findByTestId('track-tables'), {
    timeout: 5000,
  });

  // Wait for the button to appear on the screen using findBy. Store it in a variable to click it afterwards.
  const button = await canvas.findByTestId(`track-public.Invoice`);

  // Wait an additional second, otherwise clicking does not fire the request
  // Tried to figure out how to avoid using delay and favor waitFor and await findBy,
  // but could not find a visual cue that indicates that clicking the button will work
  // This might indicate that the button must wait for some asynchronous operation before it's ready
  await dangerouslyDelay(1000);

  // Track public.Invoice
  userEvent.click(button);

  // It should not be in the Untracked tab anymore
  await waitForElementToBeRemoved(
    () => canvas.queryByText('public / Invoice'),
    {
      timeout: 2000,
    }
  );
};

export const TrackedTables: ComponentStory<typeof ManageTrackedTables> = () => (
  <ManageTrackedTables dataSourceName="chinook" />
);

TrackedTables.storyName = 'Test - Track all tables 🧪';

TrackedTables.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Reset initial metadata to make sure tests start from a clean slate everytime
  resetMetadata();

  // Wait until it finishes loading
  await waitFor(() => canvas.findByTestId('track-tables'), {
    timeout: 5000,
  });

  await userEvent.click(await canvas.findByText('Tracked'));

  // Verify it correctly displays tracked tables
  await expect(canvas.queryByText('public / Artist')).toBeInTheDocument();
  await expect(canvas.queryByText('public / Album')).toBeInTheDocument();
  await expect(canvas.queryByText('public / Employee')).toBeInTheDocument();
  await expect(canvas.queryByText('public / Customer')).toBeInTheDocument();
  await expect(canvas.queryByText('public / Genre')).toBeInTheDocument();
};

export const MassiveTableAmount = UntrackedTables.bind({});

MassiveTableAmount.parameters = {
  msw: handlers(1000000),
};
