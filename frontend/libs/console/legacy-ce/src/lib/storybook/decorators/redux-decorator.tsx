import { configureStore } from '@reduxjs/toolkit';
import merge from 'lodash/merge';
import { Provider } from 'react-redux';
import reducer from '../../reducer';
import { RootState } from '../../store';
import { DeepPartial } from '../../types';
import { Decorator } from '@storybook/react';

export const ReduxDecorator = (
  mockValues: DeepPartial<RootState>
): Decorator => {
  // Dispatch manually inside reducer with undefined as state and a action that shouldn't be used
  // So we have the base state of the app
  const baseState = reducer(undefined, { type: '@@INIT_MOCK_STORYBOOK@@' });

  // Deep merge to override any specific values we need for the stories
  const state = merge(baseState, mockValues);
  const store = configureStore({
    reducer,

    // Inject it in the preloadedState, the original purpose of this is for hydratation
    preloadedState: state,

    // Enable redux browser devtools
    devTools: true,
  });

  return Story => (
    <Provider store={store}>
      <Story />
    </Provider>
  );
};
