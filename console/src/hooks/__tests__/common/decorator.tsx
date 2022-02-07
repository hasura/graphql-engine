import reducer from '@/reducer';
import { RootState } from '@/store';
import { DeepPartial } from '@/types';
import { configureStore } from '@reduxjs/toolkit';
import merge from 'lodash.merge';
import React from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { Provider } from 'react-redux';

const HookTestProvider: React.FC<{
  mockValues?: DeepPartial<RootState>;
}> = ({ children, mockValues }) => {
  const baseState = reducer(undefined, { type: '@@INIT_MOCK_HOOKS@@' });
  const state = merge(baseState, mockValues);
  const store = configureStore({
    reducer,
    preloadedState: state,
    devTools: true,
  });

  return (
    <Provider store={store}>
      <QueryClientProvider client={new QueryClient()}>
        {children}
      </QueryClientProvider>
    </Provider>
  );
};

export const wrapper: React.FC = ({ children }) => (
  <HookTestProvider>{children}</HookTestProvider>
);
