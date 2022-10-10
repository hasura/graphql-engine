import React from 'react';
import { DecoratorFn } from '@storybook/react';

import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

export const ReactQueryDecorator = (): DecoratorFn => {
  const reactQueryClient = new QueryClient();
  return Story => (
    <QueryClientProvider client={reactQueryClient}>
      <Story />
      <ReactQueryDevtools />
    </QueryClientProvider>
  );
};
