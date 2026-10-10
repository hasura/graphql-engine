import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';
import { ReactNode } from 'react';

export const reactQueryClient = new QueryClient();

export const ReactQueryProvider = ({ children }: { children: ReactNode }) => (
  <QueryClientProvider client={reactQueryClient}>
    {children}
    <ReactQueryDevtools />
  </QueryClientProvider>
);
