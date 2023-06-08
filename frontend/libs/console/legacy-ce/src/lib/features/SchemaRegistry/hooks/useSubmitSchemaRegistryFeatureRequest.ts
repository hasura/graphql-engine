import { useMutation } from 'react-query';
import { GraphQLError } from 'graphql';
import globals from '../../../Globals';
import {
  controlPlaneClient,
  ADD_SCHEMA_REGISTRY_FEATURE_REQUEST,
  AddSchemaRegistryFeatureRequestMutation,
} from '../../ControlPlane';

export const useSubmitSchemaRegistryFeatureRequest = (
  successCb: () => void,
  errorCb: (error?: GraphQLError) => void
) => {
  const addSchemaRegistryFtRequestMutation = () => {
    return controlPlaneClient.query<{
      data: AddSchemaRegistryFeatureRequestMutation;
      errors: [GraphQLError];
    }>(ADD_SCHEMA_REGISTRY_FEATURE_REQUEST, {
      details: {
        source: 'cloud-console',
        project_id: globals.hasuraCloudProjectId,
      },
    });
  };

  const mutation = useMutation(addSchemaRegistryFtRequestMutation, {
    onSuccess: data => {
      if (data.errors && data.errors.length > 0) {
        errorCb(data.errors[0]);
      } else {
        successCb();
      }
    },
    onError: () => {
      errorCb();
    },
  });

  const onSubmit = () => {
    mutation.mutate();
  };

  return {
    onSubmit,
    loading: mutation.isLoading,
  };
};
