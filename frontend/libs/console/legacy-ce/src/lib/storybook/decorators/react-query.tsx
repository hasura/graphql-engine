import { Decorator } from '@storybook/react';
import React, { useEffect } from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

const reactQueryClient = new QueryClient();
let timestamp = 'initial';

export const ReactQueryDecorator = (): Decorator => {
  return Story => (
    <ReactQueryProvider>
      <Story />
      <ReactQueryDevtools />
    </ReactQueryProvider>
  );
};

// you can't use hooks directly in a Decorator function, so we need to encapsulate this in a component
const ReactQueryProvider: React.FC = ({ children }) => {
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
