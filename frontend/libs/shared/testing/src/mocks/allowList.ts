import { AllowedMetadataTypes, Metadata } from '@hasura/shared/types';

export const allowListInitialData: Partial<Metadata['metadata']> = {
  allowlist: [
    {
      collection: 'allowed-queries',
      scope: {
        global: true,
      },
    },
    {
      collection: 'other_queries',
      scope: {
        global: false,
        roles: ['user'],
      },
    },
  ],
};

export const allowListHandlers: Partial<Record<AllowedMetadataTypes, any>> = {
  drop_collection_from_allowlist: (state, action) => {
    const { collection } = action.args as { collection: string };
    const existingCollection = (state.metadata.allowlist || []).find(
      (c) => c.collection === collection,
    );
    if (!existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `collection with name "${collection}" does not exist`,
          code: 'not-exists',
        },
      };
    }

    return {
      ...state,
      metadata: {
        ...state.metadata,
        allowlist: (state.metadata.allowlist || []).filter(
          (c) => c.collection !== collection,
        ),
      },
    };
  },
  add_collection_to_allowlist: (state, action) => {
    const { collection } = action.args as { collection: string };
    const existingCollection = (state.metadata.allowlist || []).find(
      (c) => c.collection === collection,
    );
    if (existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `collection with name "${collection}" already exists`,
          code: 'already-exists',
        },
      };
    }

    return {
      ...state,
      metadata: {
        ...state.metadata,
        allowlist: [...(state.metadata.allowlist || []), { collection }],
      },
    };
  },
  update_scope_of_collection_in_allowlist: (state, action) => {
    const { collection, scope } = action.args as {
      collection: string;
      scope: { global: false; roles: string[] };
    };
    const existingCollection = (state.metadata.allowlist || []).find(
      (c) => c.collection === collection,
    );
    if (!existingCollection) {
      return {
        status: 400,
        error: {
          path: '$.args.collection',
          error: `collection with name "${collection}" does not exist`,
          code: 'not-exists',
        },
      };
    }

    return {
      ...state,
      metadata: {
        ...state.metadata,
        allowlist: (state.metadata.allowlist || []).map((c) => {
          if (c.collection === collection) {
            return {
              ...c,
              scope,
            };
          }
          return c;
        }),
      },
    };
  },
};
