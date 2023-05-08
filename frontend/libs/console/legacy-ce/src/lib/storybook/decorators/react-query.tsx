import { DecoratorFn } from '@storybook/react';
import { useEffect } from 'react';

import { QueryClient, QueryClientProvider, useQueryClient } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

export const ReactQueryDecorator = (): DecoratorFn => {
  const reactQueryClient = new QueryClient();

  return Story => (
    <QueryClientProvider client={reactQueryClient}>
      <QueryInvalidator />
      <Story />
      <ReactQueryDevtools />
    </QueryClientProvider>
  );
};

const QueryInvalidator = () => {
  const client = useQueryClient();
  useEffect(() => {
    return () => {
      client.invalidateQueries();
    };
  }, []);

  return null;
};
