import LZString from 'lz-string';
import { Schema, SchemaChange, SchemaChangeCard } from '../types';

export const sampleChanges: SchemaChange[] = [
  {
    criticality: { level: 'BREAKING' },
    type: 'FIELD_REMOVED',
    meta: {},
    message: "Field 'email' was removed from object type 'users'.",
    path: 'users.email',
  },
  {
    criticality: { level: 'DANGEROUS' },
    type: 'ENUM_VALUE_ADDED',
    meta: {},
    message: "Enum value 'ARCHIVED' was added to enum 'status'.",
    path: 'status.ARCHIVED',
  },
  {
    criticality: { level: 'NON_BREAKING' },
    type: 'FIELD_ADDED',
    meta: {},
    message: "Field 'display_name' was added to object type 'users'.",
    path: 'users.display_name',
  },
];

export const sampleSchema: Schema = {
  id: 'published-schema',
  hash: '5c99bc01e6ae75157dc2eedb0e784c99c0cda368',
  entry_hash: 'schema-entry',
  created_at: '2026-10-20T16:30:05Z',
  tags: [],
  roleBasedSchemas: [
    {
      id: 'admin-schema',
      role: 'admin',
      hash: '5c99bc01e6ae75157dc2eedb0e784c99c0cda368',
      entry_hash: 'schema-entry',
      raw: LZString.compressToBase64(
        'type Query {\n  users: [User!]!\n}\n\ntype User {\n  id: ID!\n  display_name: String\n}',
      ),
      changes: sampleChanges,
    },
  ],
};

export const sampleHistory: SchemaChangeCard[] = [
  {
    id: sampleSchema.id,
    hash: sampleSchema.hash,
    entry_hash: sampleSchema.entry_hash,
    created_at: sampleSchema.created_at,
    tags: [],
    roles: [
      { id: 'admin-schema', role: 'admin' },
      { id: 'user-schema', role: 'user' },
    ],
  },
  {
    id: 'older-schema',
    hash: 'older-hash',
    entry_hash: 'older-entry',
    created_at: '2026-10-19T12:15:00Z',
    tags: [],
    roles: [
      { id: 'older-admin', role: 'admin' },
      { id: 'older-user', role: 'user' },
      { id: 'older-editor', role: 'editor' },
      { id: 'older-viewer', role: 'viewer' },
    ],
  },
];

export const schemaWithChanges = (
  changes: SchemaChange[] | undefined,
): Schema => ({
  ...sampleSchema,
  roleBasedSchemas: [{ ...sampleSchema.roleBasedSchemas[0], changes }],
});
