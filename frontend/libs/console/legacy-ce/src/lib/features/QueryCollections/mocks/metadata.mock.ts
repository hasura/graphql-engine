import produce from 'immer';

import { allowedMetadataTypes } from '../../MetadataAPI';
import { Metadata } from '../../hasura-metadata-types';

import { deleteAllowListQuery } from '../../../metadata/utils';
import { MetadataReducer } from '../../../mocks/actions';

export const queryCollectionInitialData: Partial<Metadata['metadata']> = {
  query_collections: [
    {
      name: 'allowed-queries',
      definition: {
        queries: [
          {
            name: 'MyQuery',
            query: `mutation update_user_by_pk($id: Int!, $object: user_set_input!) {
  update_user_by_pk(pk_columns: {id: $id}, _set: $object) {
    address
    bool
    count
    date
    email
    id
    name
    uuid
  }
}
`,
          },
          { name: 'MyQuery2', query: 'query MyQuery2 { user { email name}}' },
          { name: 'MyQuery3', query: 'query MyQuery3 { user { email name}}' },
        ],
      },
    },
    { name: 'other_queries', definition: { queries: [] } },
  ],
};

export const metadataHandlers: Partial<
  Record<allowedMetadataTypes, MetadataReducer>
> = {
  drop_query_collection: (state, action) => {
    const { collection } = (action as ReturnType<typeof deleteAllowListQuery>)
      .args;
    const existingCollection = (state.metadata.query_collections || []).find(
      c => c.name === collection
    );
    if (!existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `query collection with name "${collection}" does not exist`,
          code: 'not-exists',
        },
      };
    }
    return produce(state, draft => {
      draft.metadata.query_collections = (
        draft.metadata.query_collections || []
      ).filter(c => c.name !== collection);
    });
  },
  rename_query_collection: (state, action) => {
    const { name, new_name } = action.args as {
      name: string;
      new_name: string;
    };
    const existingCollection = (state.metadata.query_collections || []).find(
      c => c.name === name
    );
    if (!existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `query collection with name "${name}" does not exist`,
          code: 'not-exists',
        },
      };
    }
    const existingNewCollection = (state.metadata.query_collections || []).find(
      c => c.name === new_name
    );
    if (existingNewCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.new_name',
          error: `query collection with name "${new_name}" already exists`,
          code: 'already-exists',
        },
      };
    }
    return produce(state, draft => {
      draft.metadata.query_collections = (
        draft.metadata.query_collections || []
      ).map(c => {
        if (c.name === name) {
          return {
            ...c,
            name: new_name,
          };
        }
        return c;
      });
    });
  },
  create_query_collection: (state, action) => {
    const { name } = action.args as { name: string };
    const existingCollection = (state.metadata.query_collections || []).find(
      c => c.name === name
    );
    if (existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `query collection with name "${name}" already exists`,
          code: 'already-exists',
        },
      };
    }
    return produce(state, draft => {
      draft.metadata.query_collections = [
        ...(draft.metadata.query_collections || []),
        {
          name,
          definition: {
            queries: [],
          },
        },
      ];
    });
  },
  add_query_to_collection: (state, action) => {
    const {
      collection_name: collection,
      query,
      query_name,
    } = action.args as {
      collection_name: string;
      query_name: string;
      query: string;
    };
    const existingCollection = (state.metadata.query_collections || []).find(
      c => c.name === collection
    );
    if (!existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `query collection with name "${collection}" does not exist`,
          code: 'not-exists',
        },
      };
    }
    const existingQuery = existingCollection.definition.queries.find(
      q => q.name === query_name
    );
    if (existingQuery) {
      return {
        status: 400,
        error: {
          path: '$.args.query',
          error: `query with name "${query_name}" already exists in collection "${collection}"`,
          code: 'already-exists',
        },
      };
    }
    return produce(state, draft => {
      draft.metadata.query_collections = (
        draft.metadata.query_collections || []
      ).map(c => {
        if (c.name === collection) {
          return {
            ...c,
            definition: {
              queries: [
                ...c.definition.queries,
                {
                  name: query_name,
                  query,
                },
              ],
            },
          };
        }
        return c;
      });
    });
  },
  drop_query_from_collection: (state, action) => {
    const { collection_name: collection, query_name } = action.args as {
      collection_name: string;
      query_name: string;
    };
    const existingCollection = (state.metadata.query_collections || []).find(
      c => c.name === collection
    );
    if (!existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `query collection with name "${collection}" does not exist`,
          code: 'not-exists',
        },
      };
    }
    const existingQuery = existingCollection.definition.queries.find(
      q => q.name === query_name
    );
    if (!existingQuery) {
      return {
        status: 400,
        error: {
          path: '$.args.query',
          error: `query with name "${query_name}" does not exist in collection "${collection}"`,
          code: 'not-exists',
        },
      };
    }
    return produce(state, draft => {
      draft.metadata.query_collections = (
        draft.metadata.query_collections || []
      ).map(c => {
        if (c.name === collection) {
          return {
            ...c,
            definition: {
              queries: c.definition.queries.filter(q => q.name !== query_name),
            },
          };
        }
        return c;
      });
    });
  },
};
