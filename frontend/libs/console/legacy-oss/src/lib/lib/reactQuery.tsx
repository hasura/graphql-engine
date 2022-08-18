import React from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

export const reactQueryClient = new QueryClient();

export const ReactQueryProvider: React.FC = ({ children }) => (
  <QueryClientProvider client={reactQueryClient}>
    {children}
    <ReactQueryDevtools />
  </QueryClientProvider>
);
