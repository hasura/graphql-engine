import React from 'react';
import { IconTooltip } from '../../../../new-components/Tooltip';
import { useFormContext } from 'react-hook-form';
import { FaShieldAlt } from 'react-icons/fa';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

export const GraphQLServiceUrl = () => {
  const { register } = useFormContext();
  return (
    <div className="mb-md w-6/12">
      <label className="block flex items-center text-gray-600 font-semibold mb-xs">
        GraphQL Service URL
        <IconTooltip
          message="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
          icon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
        />
        <LearnMoreLink href="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl" />
      </label>
      <p className="text-sm text-gray-600 mb-sm">
        Note: Provide an URL or use an env var to template the handler URL if
        you have different URLs for multiple environments.
      </p>
      <div className="flex shadow-sm rounded">
        <input
          type="text"
          className="flex-1 min-w-0 block w-full px-3 py-2 rounded-r border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          placeholder="https://myservice.com/graphql or {{MY_WEBHOOK_URL}}/graphql"
          {...register('url.value')}
          data-testid="url"
        />
      </div>
    </div>
  );
};
