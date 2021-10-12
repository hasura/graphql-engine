import React from 'react';
import { Provider } from 'react-redux';
import { addDecorator } from '@storybook/react';
import { initializeWorker, mswDecorator } from 'msw-storybook-addon';
import '../src/theme/tailwind.css';
import { store } from '../src/store';

export const parameters = {
  actions: { argTypesRegex: '^on[A-Z].*' },
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
};

export const decorators = [
  (fn, c) => <Provider store={store}>{fn(c)}</Provider>,
];

initializeWorker();
addDecorator(mswDecorator);
