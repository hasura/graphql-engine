import type { StoryObj, Meta } from '@storybook/react';

import * as React from 'react';

import produce from 'immer';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { ReduxDecorator } from '../../../storybook/decorators/redux-decorator';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

import {
  createDefaultInitialData,
  handlers,
} from '../../../mocks/metadata.mock';

import { OpenTelemetryProvider } from './OpenTelemetryProvider';

// --------------------------------------------------
// NOT TESTED
// --------------------------------------------------
// The following scenarios do not have interaction tests because...:
// - The success and error notification: because it's better to test it through a browser test until showing the notifications in Storybook is not possible

export default {
  title: 'Features/OpenTelemetry/OpenTelemetryProvider',
  component: OpenTelemetryProvider,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({
      // Allows to see the loading states
      delay: 500,

      // This story requires just the OpenTelemetry-related metadata handlers
      initialData: produce(createDefaultInitialData(), draft => {
        draft.metadata.opentelemetry = undefined;
      }),
    }),
  },
} as Meta<typeof OpenTelemetryProvider>;

export const Default: StoryObj<typeof OpenTelemetryProvider> = {
  render: () => {
    return <OpenTelemetryProvider />;
  },

  name: '💠 Default',
};

export const HappyPath: StoryObj<typeof OpenTelemetryProvider> = {
  render: () => <OpenTelemetryProvider />,

  name: '🧪 Testing - When enable OpenTelemetry, it should update the OpenTelemetry metadata',

  parameters: {
    chromatic: { disableSnapshot: true },
    msw: handlers({
      // Speeds up the test as much as possible
      delay: 0,

      // This story requires just the OpenTelemetry-related metadata handlers
      initialData: produce(createDefaultInitialData(), draft => {
        draft.metadata.opentelemetry = undefined;
      }),
    }),
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // STEP: Wait until the metadata has been loaded (through waiting for the submit button being enabled)
    const submitButton = await canvas.findByRole('button', { name: 'Connect' });
    await waitFor(() => {
      expect(submitButton).toBeEnabled();
    });

    // STEP: Check the badge shows OpenTelemetry is disabled
    const badge = await canvas.findByTestId('badge');
    expect(badge).toHaveTextContent('Disabled');

    // act avoids the "When testing, code that causes React state updates should be wrapped into act(...):" error

    // STEP: Enable OpenTelemetry
    await userEvent.click(await canvas.findByLabelText('Status'));

    // STEP: Type the Endpoint
    await userEvent.type(
      await canvas.findByLabelText('Endpoint', { selector: 'input' }),
      'http://hasura.io'
    );

    // STEP: Click the Submit button
    await userEvent.click(submitButton);

    // STEP: Wait for OpenTelemetry to be enabled (through waiting for the badge to show "Enabled"
    // since the badge update only after updating the metadata and reloading it)
    await waitFor(async () => {
      expect(await canvas.findByTestId('badge')).toHaveTextContent('Enabled');
    });
  },
};
