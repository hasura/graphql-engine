import { store } from '../../../store';
import { render } from '@testing-library/react';
import React from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { Provider } from 'react-redux';

const HookTestProvider: React.FC = ({ children }) => {
  return (
    <Provider store={store}>
      <QueryClientProvider
        client={
          new QueryClient({
            defaultOptions: {
              queries: {
                retry: false,
              },
            },
          })
        }
      >
        {children}
      </QueryClientProvider>
    </Provider>
  );
};

export const wrapper: React.FC = ({ children }) => (
  <HookTestProvider>{children}</HookTestProvider>
);

export function renderWithClient(ui: React.ReactElement) {
  const { rerender, ...result } = render(
    <HookTestProvider>{ui}</HookTestProvider>
  );
  return {
    ...result,
    rerender: (rerenderUi: React.ReactElement) =>
      rerender(<HookTestProvider>{rerenderUi}</HookTestProvider>),
  };
}
