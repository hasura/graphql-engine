import produce from 'immer';

import { allowedMetadataTypes } from '../../MetadataAPI';
import { Metadata, RestEndpoint } from '../../hasura-metadata-types';
import { MetadataReducer } from '../../../mocks/actions';

export const restEndpointsInitialData: Partial<Metadata['metadata']> = {
  rest_endpoints: [
    {
      comment: 'Description of my rest endpoint',
      definition: {
        query: {
          collection_name: 'allowed-queries',
          query_name: 'MyQuery',
        },
      },
      methods: ['POST'],
      name: 'MyQuery',
      url: 'test/:id',
    },
  ],
};

export const metadataHandlers: Partial<
  Record<allowedMetadataTypes, MetadataReducer>
> = {
  create_rest_endpoint: (state, action) => {
    const endpoint = action.args as RestEndpoint;
    const existingEndpoint = (state.metadata.rest_endpoints || []).find(
      c => c.name === endpoint.name
    );
    if (existingEndpoint) {
      return {
        status: 400,
        error: {
          path: '$.args.name',
          error: `rest endpoint with name "${endpoint.name}" already exists`,
          code: 'already-exists',
        },
      };
    }
    const existingQueryCollection = (
      state.metadata.query_collections || []
    ).find(c => c.name === endpoint.definition.query.collection_name);
    if (!existingQueryCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.query.collection_name',
          error: `query collection with name "${endpoint.definition.query.collection_name}" does not exist`,
          code: 'not-exists',
        },
      };
    }
    const existingQuery = (
      existingQueryCollection.definition?.queries || []
    ).find(q => q.name === endpoint.definition.query.query_name);
    if (!existingQuery) {
      return {
        status: 400,
        error: {
          path: '$.args.query.query_name',
          error: `query with name "${endpoint.definition.query.query_name}" does not exist in query collection "${endpoint.definition.query.collection_name}"`,
          code: 'not-exists',
        },
      };
    }
    return produce(state, draft => {
      draft.metadata.rest_endpoints = [
        ...(draft.metadata.rest_endpoints || []),
        endpoint,
      ];
    });
  },
};
