import { allowedMetadataTypes } from '@/features/MetadataAPI';
import { Metadata } from '@/features/hasura-metadata-types';

import { metadataHandlers as allowListMetadataHandlers } from '@/features/AllowLists';
import { metadataHandlers as queryCollectionMetadataHandlers } from '@/features/QueryCollections';

import { TMigration } from '../features/MetadataAPI/hooks/useMetadataMigration';

export type ResponseBodyMetadataTypeError = {
  code: string;
  error: string;
  path: string;
};

export type MetadataAction = TMigration['query'];
export type MetadataReducer = (
  state: Metadata,
  action: MetadataAction
) =>
  | Metadata
  | {
      status: number;
      error: ResponseBodyMetadataTypeError;
    };

const metadataHandlers: Partial<Record<allowedMetadataTypes, MetadataReducer>> =
  {
    export_metadata: state => state,
    ...allowListMetadataHandlers,
    ...queryCollectionMetadataHandlers,
  };

export const metadataReducer: MetadataReducer = (state, action) => {
  if (action.type === 'bulk') {
    let reduceFirstError: ResponseBodyMetadataTypeError;
    return action.args.reduce((currentState: Metadata, arg: MetadataAction) => {
      if (reduceFirstError) {
        return currentState;
      }
      const response = metadataReducer(currentState, arg);
      if ('error' in response) {
        reduceFirstError = response.error;
      }
      return response;
    }, state);
  }
  const handler = metadataHandlers[action.type];
  if (handler) {
    return handler(state, action);
  }
  return {
    status: 400,
    error: {
      path: '$.type',
      error: `unknown type "${action.type}"`,
      code: 'unknown-type',
    },
  };
};
