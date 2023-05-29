import { Decorator } from '@storybook/react';
import React from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';
import useUpdateEffect from '../../hooks/useUpdateEffect';

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
  let reactQueryClient = new QueryClient();

  useUpdateEffect(() => {
    // re-render happens only on story change or HMR. in those cases we want a fresh instance of the query client so we don't end up with stale cache
    // invalidating queries is buggy due to async behavior and can cause storybook to lock up.
    // so, this is the safest approach.
    reactQueryClient = new QueryClient();
  });
  return (
    <QueryClientProvider client={reactQueryClient}>
      {children}
    </QueryClientProvider>
  );
};
