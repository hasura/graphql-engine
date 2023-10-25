import React from 'react';

import { StoryFn, Meta } from '@storybook/react';
import { RelationshipMapping } from './RelationshipMapping';

export default {
  component: RelationshipMapping,
} as Meta<typeof RelationshipMapping>;

export const LocalRelationship: StoryFn<typeof RelationshipMapping> = () => (
  <RelationshipMapping
    relationship={{
      name: 'Album_Artist',
      fromSource: 'aMySQL',
      fromTable: ['Album'],
      type: 'localRelationship',
      relationshipType: 'Object',
      definition: { toTable: ['Artist'], mapping: { ArtistId: 'ArtistId' } },
    }}
  />
);

export const RemoteDatabaseRelationship: StoryFn<
  typeof RelationshipMapping
> = () => (
  <RelationshipMapping
    relationship={{
      name: 'ar',
      fromSource: 'aMySQL',
      fromTable: ['Artist'],
      relationshipType: 'Object',
      type: 'remoteDatabaseRelationship',
      definition: {
        toSource: 'aPostgres',
        toTable: { name: 'Album', schema: 'public' },
        mapping: { ArtistId: 'ArtistId' },
      },
    }}
  />
);

export const RemoteSchemaRelationship: StoryFn<
  typeof RelationshipMapping
> = () => (
  <RelationshipMapping
    relationship={{
      name: 'RemoteSchema',
      fromSource: 'aMySQL',
      fromTable: ['Customer'],
      relationshipType: 'Remote',
      type: 'remoteSchemaRelationship',
      definition: {
        toRemoteSchema: 'Pokemon',
        lhs_fields: ['CustomerId'],
        remote_field: {
          pokemon_v2_ability_by_pk: { arguments: { id: '$CustomerId' } },
        },
      },
    }}
  />
);
