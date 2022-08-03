import React from 'react';
import { IconTooltip } from '@/new-components/Tooltip';
import { useFormContext } from 'react-hook-form';

export const GraphQLServiceUrl = () => {
  const { watch, register } = useFormContext();
  const url = watch('url');
  return (
    <div className="mb-md w-6/12">
      <label className="block flex items-center text-gray-600 font-semibold mb-xs">
        GraphQL Service URL
        <IconTooltip message="Remote GraphQL server’s URL. E.g. https://my-domain/v1/graphql" />
      </label>
      <p className="text-sm text-gray-600 mb-sm">
        Note: Specifying the server URL via an environmental variable is
        recommended if you have different URLs for multiple environments.
      </p>
      <div className="flex shadow-sm rounded">
        <select
          className="inline-flex rounded-l border border-r-0 border-gray-300 bg-white hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          {...register('url.type')}
        >
          <option value="from_url">URL</option>
          <option value="from_env">Env Var</option>
        </select>
        <input
          type="text"
          className="flex-1 min-w-0 block w-full px-3 py-2 rounded-r border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          placeholder={
            url?.type === 'from_url'
              ? 'https://myservice.com/graphql'
              : 'MY_GRAPHQL_ENDPOINT'
          }
          {...register('url.value')}
          data-testid="url"
        />
      </div>
    </div>
  );
};
