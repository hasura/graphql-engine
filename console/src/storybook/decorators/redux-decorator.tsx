import React from 'react';
import { DecoratorFn } from '@storybook/react';
import { Provider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
import merge from 'lodash.merge';

import { RootState } from '../../store';
import reducer from '../../reducer';
import { DeepPartial } from '../../types';

export const ReduxDecorator = (
  mockValues: DeepPartial<RootState>
): DecoratorFn => {
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

  return story => <Provider store={store}>{story()}</Provider>;
};
