import type { Meta, StoryObj } from '@storybook/react';

import produce from 'immer';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../storybook/decorators/redux-decorator';

import {
  createDefaultInitialData,
  handlers,
} from '../../../mocks/metadata.mock';

import { ConsoleTypeDecorator } from '../../../storybook/decorators';
import { OpenTelemetryEEProvider } from './OpenTelemetryEEProvider';

// --------------------------------------------------
// NOT TESTED
// --------------------------------------------------
// The following scenarios do not have interaction tests because...:
// - The success and error notification: because it's better to test it through a browser test until showing the notifications in Storybook is not possible

export default {
  title: 'Features/OpenTelemetry/OpenTelemetryProvider',
  component: OpenTelemetryEEProvider,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro' }),
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
} as Meta<typeof OpenTelemetryEEProvider>;

export const Default: StoryObj<typeof OpenTelemetryEEProvider> = {
  render: () => {
    return <OpenTelemetryEEProvider />;
  },

  name: '💠 Default',
};

export const HappyPath: StoryObj<typeof OpenTelemetryEEProvider> = {
  render: () => <OpenTelemetryEEProvider />,

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
  // test is broken behind an "Enabled Enterprise" Banner. Unclear how best to fix text.
  // play: async ({ canvasElement }) => {
  //   const canvas = within(canvasElement);

  //   // STEP: Wait until the metadata has been loaded (through waiting for the submit button being enabled)
  //   const submitButton = await canvas.findByRole('button', { name: 'Connect' });
  //   await waitFor(() => {
  //     expect(submitButton).toBeEnabled();
  //   });

  //   // STEP: Check the badge shows OpenTelemetry is disabled
  //   const badge = await canvas.findByTestId('badge');
  //   expect(badge).toHaveTextContent('Disabled');

  //   // act avoids the "When testing, code that causes React state updates should be wrapped into act(...):" error

  //   // STEP: Enable OpenTelemetry
  //   await userEvent.click(await canvas.findByLabelText('Status'));

  //   // STEP: Type the Endpoint
  //   await userEvent.type(
  //     await canvas.findByLabelText('Endpoint', { selector: 'input' }),
  //     'http://hasura.io'
  //   );

  //   // STEP: Click the Submit button
  //   await userEvent.click(submitButton);

  //   // STEP: Wait for OpenTelemetry to be enabled (through waiting for the badge to show "Enabled"
  //   // since the badge update only after updating the metadata and reloading it)
  //   await waitFor(async () => {
  //     expect(await canvas.findByTestId('badge')).toHaveTextContent('Enabled');
  //   });
  // },
};
