import React from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

export const reactQueryClient = new QueryClient();

/**
 * This is needed by the redux store to trigger
 * invalidate queries when the metadata is updated in the store.
 */
window.reactQueryClient = reactQueryClient;

export const ReactQueryProvider: React.FC = ({ children }) => (
  <QueryClientProvider client={reactQueryClient}>
    {children}
    <ReactQueryDevtools />
  </QueryClientProvider>
);
