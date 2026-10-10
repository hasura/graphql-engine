import { Decorator } from '@storybook/react';
import React, { useEffect } from 'react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';

const reactQueryClient = new QueryClient({
  defaultOptions: {
    queries: {
      gcTime: 0,
    },
  },
});
let timestamp = 'initial';

export const ReactQueryDecorator = (): Decorator => {
  return (Story) => (
    <ReactQueryProvider>
      <Story />
      <ReactQueryDevtools />
    </ReactQueryProvider>
  );
};

// you can't use hooks directly in a Decorator function, so we need to encapsulate this in a component
const ReactQueryProvider: React.FC<{ children?: React.ReactNode }> = ({
  children,
}) => {
  useEffect(() => {
    if (timestamp !== 'initial') {
      // this resets all active queries so when HMR or a story changes and uses the same query key, it won't be able to serve stale data.
      reactQueryClient.resetQueries();
    }
    timestamp = new Date().toISOString();
  }, []);

  return (
    <QueryClientProvider client={reactQueryClient}>
      {children}
    </QueryClientProvider>
  );
};
