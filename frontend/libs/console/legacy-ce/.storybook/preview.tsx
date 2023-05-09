import React from 'react';
import { Provider } from 'react-redux';
import { initialize, mswDecorator } from 'msw-storybook-addon';
import mockdate from 'mockdate';
import dayjs from 'dayjs';
import theme from './theme';
import 'react-loading-skeleton/dist/skeleton.css';
import '../src/lib/theme/tailwind.css';
import { store } from '../src/lib/store';
import '../src/lib/components/Common/Common.module.scss';
import { ToastsHub } from '../src/lib/new-components/Toasts';
import { AlertProvider } from '../src/lib/new-components/Alert/AlertProvider';

initialize();

export const parameters = {
  // Add a minimum of 300 ms of delay for ui testing
  actions: { argTypesRegex: '^on.*' },
  options: {
    storySort: {
      order: ['Design system', 'Dev', 'Components', 'Hooks'],
    },
  },
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
  darkMode: {
    dark: { ...theme.dark },
    light: { ...theme.light },
  },
};

export const decorators = [
  (fn, c) => <Provider store={store}>{fn(c)}</Provider>,
  mswDecorator,
  (story, { parameters }) => {
    mockdate.reset();

    if (!parameters.mockdate) {
      return story();
    }

    // Allows us to "time travel" to ensure our stories don't change over time
    mockdate.set(parameters.mockdate);

    const mockedDate = dayjs(parameters.mockdate).format('DD-MM-YYYY HH:mma');

    return (
      <div>
        {story()}
        <div
          style={{
            position: 'fixed',
            bottom: 0,
            right: 0,
            background: 'rgba(0, 0, 0, 0.15)',
            padding: '5px',
            lineHeight: 1,
          }}
        >
          <span className={'font-bold'}>Mocked date:</span> {mockedDate}
        </div>
      </div>
    );
  },
  Story => {
    document.body.classList.add('hasura-tailwind-on');
    return (
      <AlertProvider>
        <div>
          <ToastsHub />
          <div className={'bg-legacybg'}>
            <Story />
          </div>
        </div>
      </AlertProvider>
    );
  },
];

export const argTypes = {
  disableSnapshotTesting: {
    table: {
      disable: true,
    },
  },
};
