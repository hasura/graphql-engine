import { render } from '@testing-library/react';
import React from 'react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router';
import {
  AppContext,
  defaultAppState,
  getEndpoints,
} from '@hasura/shared/context';

const HookTestProvider: React.FC<{ children?: React.ReactNode }> = ({
  children,
}) => {
  return (
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
      {/*
        MemoryRouter is required because data hooks reach for router context
        (e.g. `useAuthFetchJson` calls `useLocation`/`useNavigate` to redirect
        to the login page on 401).
      */}
      <MemoryRouter>
        <AppContext.Provider
          value={{
            ...defaultAppState,
            envVars: (window as any).__env ?? {},
            endpoints: getEndpoints(
              (window as any).__env ?? {},
              'http://localhost',
            ),
          }}
        >
          {children}
        </AppContext.Provider>
      </MemoryRouter>
    </QueryClientProvider>
  );
};

export const testWrapper: React.FC<{ children?: React.ReactNode }> = ({
  children,
}) => <HookTestProvider>{children}</HookTestProvider>;

export function testRenderWithClient(ui: React.ReactElement<any>) {
  const { rerender, ...result } = render(
    <HookTestProvider>{ui}</HookTestProvider>,
  );
  return {
    ...result,
    rerender: (rerenderUi: React.ReactElement<any>) =>
      rerender(<HookTestProvider>{rerenderUi}</HookTestProvider>),
  };
}
