import * as React from 'react';
import { Button } from '@hasura/shared/ui';
import { GraphQLError } from 'graphql';
import { hasuraToast } from '@hasura/shared/ui';
import { useSubmitSchemaRegistryFeatureRequest } from '../hooks/useSubmitSchemaRegistryFeatureRequest';
import { SCHEMA_REGISTRY_REF_URL } from '../constants';
import { Flex } from '@radix-ui/themes';

export const FeatureRequest = () => {
  const onSuccess = () => {
    hasuraToast({
      title: 'We have received your feature request for Schema Registry.',
      message: 'Our team will enable the feature for you soon.',
      type: 'success',
    });
  };

  const onError = (error?: GraphQLError) => {
    const errorMessage = error?.message || '';
    if (errorMessage.includes('request already submitted')) {
      hasuraToast({
        title: 'We have received your feature request for Schema Registry.',
        message: 'Our team will enable the feature for you soon.',
        type: 'success',
      });
    } else {
      hasuraToast({
        title: 'Error requesting feature access',
        message: 'Please try again in some time.',
        type: 'error',
      });
    }
  };

  const { onSubmit, loading } = useSubmitSchemaRegistryFeatureRequest(
    onSuccess,
    onError,
  );

  const onRequest = () => {
    onSubmit();
  };

  return (
    <div className="bg-white w-[50%] p-4 border-neutral-200 semi-rounded">
      <span className="mb-2 text-muted">
        The Hasura Schema Registry work is aimed to make your Hasura GraphQL
        schema changes more reliable, prevent breaking changes in your schema
        and make collaboration across large teams, micro services and roles much
        more manageable and predictable.{' '}
        <a
          href={SCHEMA_REGISTRY_REF_URL}
          target="_blank"
          rel="noreferrer noopener"
        >
          {' '}
          Read more.
        </a>
      </span>
      <div className="mb-2">
        <img
          src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/cloud-console/assets/common/img/schema-registry-preview-screenshot.png"
          alt="Schema Registry Preview Image"
        />
      </div>
      <Flex justify="start" align="center" className="w-full">
        <Button
          mode="primary"
          className={`mr-2 ${loading ? 'cursor-not-allowed' : ''}`}
          disabled={loading}
          onClick={onRequest}
        >
          {loading ? 'Requesting...' : 'Request access'}
        </Button>
        <h4 className="font-italics text-muted">
          This feature is currently in closed alpha.
        </h4>
      </Flex>
    </div>
  );
};
