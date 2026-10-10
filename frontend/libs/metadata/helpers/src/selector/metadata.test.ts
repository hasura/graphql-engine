import { Metadata } from '@hasura/shared/types';
import {
  findFunction,
  findLogicalModel,
  findMetadataFunction,
  findMetadataSource,
  findMetadataSourceSchemaNames,
  findMetadataTable,
  findNativeQuery,
  findRemoteSchema,
  findRemoteSchemaRelationship,
  findSource,
  findTable,
  getAllRemoteSchemaRelationships,
  getForeignKeyRelationships,
  getLocalDBArrayRelationships,
  getLocalDBObjectRelationships,
  getNewRolePermission,
  getOperationsFromQueryCollection,
  getRemoteDatabaseRelationships,
  getRemoteDatabaseRelationshipsFromSource,
  getRoles,
  getSources,
  getSupportsForeignKeys,
  getTables,
  isCollectionInAllowlist,
  resourceVersion,
  selectEventsTriggersByTable,
  selectEventTriggerByName,
  selectEventTriggers,
  selectForeignKeyRelationships,
  selectFunctions,
  selectManualEventsTriggers,
  selectRawEventTriggers,
  selectRoles,
} from './metadata';

const objectFk = {
  name: 'album_object_fk',
  using: {
    foreign_key_constraint_on: {
      table: ['public', 'Artist'],
      columns: ['artist_id'],
    },
  },
};
const objectManual = {
  name: 'album_object_manual',
  using: {
    manual_configuration: {
      remote_table: ['public', 'Artist'],
      column_mapping: { artist_id: 'id' },
    },
  },
};
const arrayFk = {
  name: 'album_array_fk',
  using: {
    foreign_key_constraint_on: {
      table: ['public', 'Track'],
      columns: ['album_id'],
    },
  },
};
const arrayManual = {
  name: 'album_array_manual',
  using: {
    manual_configuration: {
      remote_table: ['public', 'Track'],
      column_mapping: { id: 'album_id' },
    },
  },
};

const toSourceRel = {
  name: 'album_to_other_db',
  definition: {
    to_source: {
      relationship_type: 'object',
      field_mapping: { id: 'album_id' },
      source: 'other_db',
      table: { schema: 'public', name: 'RemoteAlbum' },
    },
  },
};
const toRemoteSchemaRel = {
  name: 'album_to_remote_schema',
  definition: {
    to_remote_schema: {
      remote_field: {},
      lhs_fields: ['id'],
      remote_schema: 'my_remote',
    },
  },
};

const albumInsertTrigger = {
  name: 'album_insert_trigger',
  definition: { enable_manual: false, insert: { columns: '*' } },
  retry_conf: { num_retries: 0 },
};
const albumManualTrigger = {
  name: 'album_manual_trigger',
  definition: { enable_manual: true },
  retry_conf: { num_retries: 0 },
};

const metadata = {
  resource_version: 42,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
        configuration: {},
        tables: [
          {
            table: { schema: 'public', name: 'Album' },
            select_permissions: [
              { role: 'user', permission: { columns: '*' } },
            ],
            insert_permissions: [{ role: 'editor', permission: { check: {} } }],
            object_relationships: [objectFk, objectManual],
            array_relationships: [arrayFk, arrayManual],
            remote_relationships: [toSourceRel, toRemoteSchemaRel],
            event_triggers: [albumInsertTrigger, albumManualTrigger],
          },
          {
            table: { schema: 'public', name: 'Artist' },
            delete_permissions: [{ role: 'admin', permission: { filter: {} } }],
            event_triggers: [],
          },
        ],
        functions: [
          { function: ['public', 'search_albums'] },
          { function: ['analytics', 'report'] },
        ],
        logical_models: [
          {
            name: 'AlbumModel',
            fields: [],
            select_permissions: [
              {
                role: 'lm_reader',
                permission: { columns: ['id'], filter: {} },
              },
            ],
          },
        ],
        native_queries: [
          {
            root_field_name: 'albumQuery',
            code: 'SELECT 1',
            returns: 'AlbumModel',
          },
        ],
      },
      {
        name: 'bq',
        kind: 'bigquery',
        configuration: {},
        tables: [
          {
            table: { dataset: 'analytics', name: 'events' },
            event_triggers: [],
          },
        ],
      },
    ],
    remote_schemas: [
      {
        name: 'my_remote',
        definition: { url: 'http://example.com' },
        permissions: [{ role: 'rs_role', definition: { schema: '' } }],
        remote_relationships: [],
      },
    ],
    actions: [{ name: 'insertUser', permissions: [{ role: 'action_role' }] }],
    allowlist: [
      {
        collection: 'allowed',
        scope: { global: false, roles: ['allow_role'] },
      },
      { collection: 'global_one', scope: { global: true } },
    ],
    query_collections: [
      {
        name: 'allowed',
        definition: { queries: [{ name: 'q1', query: 'query { a }' }] },
      },
    ],
    api_limits: {
      disabled: false,
      depth_limit: {
        global: 5,
        per_role: { limit_role: 10 },
        state: 'enabled',
      },
    },
    graphql_schema_introspection: { disabled_for_roles: ['introspect_role'] },
  },
} as unknown as Metadata;

const requireSource = (name: string) => {
  const source = findMetadataSource(name, metadata);
  if (!source) {
    throw new Error(`fixture is missing source "${name}"`);
  }
  return source;
};

const requireAlbumTable = () => {
  const table = findMetadataTable(
    'chinook',
    { schema: 'public', name: 'Album' },
    metadata,
  );
  if (!table) {
    throw new Error('fixture is missing the Album table');
  }
  return table;
};

describe('getSources', () => {
  it('returns all sources', () => {
    expect(getSources()(metadata).map((s) => s.name)).toEqual([
      'chinook',
      'bq',
    ]);
  });

  it('returns an empty array for undefined metadata', () => {
    expect(getSources()(undefined)).toEqual([]);
  });
});

describe('findMetadataSource / findSource', () => {
  it('finds a source by name', () => {
    expect(findMetadataSource('chinook', metadata)?.kind).toBe('postgres');
  });

  it('returns undefined for an unknown source', () => {
    expect(findMetadataSource('nope', metadata)).toBeUndefined();
  });

  it('findSource returns undefined when no name is given', () => {
    expect(findSource(undefined)(metadata)).toBeUndefined();
  });

  it('findSource finds by name', () => {
    expect(findSource('bq')(metadata)?.kind).toBe('bigquery');
  });
});

describe('getTables', () => {
  it('returns the tables of a source', () => {
    expect(getTables('chinook')(metadata)).toHaveLength(2);
  });

  it('returns an empty array for an unknown source', () => {
    expect(getTables('nope')(metadata)).toEqual([]);
  });
});

describe('findMetadataTable / findTable', () => {
  it('finds a table by exact shape', () => {
    expect(
      findMetadataTable(
        'chinook',
        { schema: 'public', name: 'Album' },
        metadata,
      ),
    ).toBeDefined();
  });

  it('does not find across differing shapes (uses areTablesEqual)', () => {
    // areTablesEqual is structural, so a GDC array will not match a schema object.
    expect(
      findMetadataTable('chinook', ['public', 'Album'], metadata),
    ).toBeUndefined();
  });

  it('findTable delegates to findMetadataTable', () => {
    expect(
      findTable('chinook', { schema: 'public', name: 'Artist' })(metadata),
    ).toBeDefined();
  });
});

describe('selectFunctions / findMetadataFunction / findFunction', () => {
  it('returns all functions of a source', () => {
    expect(selectFunctions('chinook')(metadata)).toHaveLength(2);
  });

  it('filters functions by schema name', () => {
    const fns = selectFunctions('chinook', 'analytics')(metadata);
    expect(fns).toHaveLength(1);
    expect(fns[0].function).toEqual(['analytics', 'report']);
  });

  it('finds a function by exact shape', () => {
    expect(
      findMetadataFunction('chinook', ['public', 'search_albums'], metadata),
    ).toBeDefined();
  });

  it('findFunction returns undefined for an unknown function', () => {
    expect(
      findFunction('chinook', ['public', 'nope'])(metadata),
    ).toBeUndefined();
  });
});

describe('findNativeQuery / findLogicalModel', () => {
  it('finds a native query by root_field_name', () => {
    expect(findNativeQuery('chinook', 'albumQuery')(metadata)?.code).toBe(
      'SELECT 1',
    );
  });

  it('finds a logical model by name', () => {
    expect(findLogicalModel('chinook', 'AlbumModel')(metadata)?.name).toBe(
      'AlbumModel',
    );
  });

  it('returns undefined for unknown names', () => {
    expect(findNativeQuery('chinook', 'nope')(metadata)).toBeUndefined();
    expect(findLogicalModel('chinook', 'nope')(metadata)).toBeUndefined();
  });
});

describe('resourceVersion', () => {
  it('returns the resource version', () => {
    expect(resourceVersion()(metadata)).toBe(42);
  });
});

describe('isCollectionInAllowlist', () => {
  it('is true for a collection in the allowlist', () => {
    expect(isCollectionInAllowlist('allowed')(metadata)).toBe(true);
  });

  it('is false for a collection not in the allowlist', () => {
    expect(isCollectionInAllowlist('nope')(metadata)).toBe(false);
  });
});

describe('getNewRolePermission', () => {
  it('returns the roles for a non-global allowlist scope', () => {
    expect(getNewRolePermission('allowed')(metadata)).toEqual(['allow_role']);
  });

  it('returns an empty array for a global scope', () => {
    expect(getNewRolePermission('global_one')(metadata)).toEqual([]);
  });

  it('returns an empty array for an unknown collection', () => {
    expect(getNewRolePermission('nope')(metadata)).toEqual([]);
  });
});

describe('getLocalDBObjectRelationships / getLocalDBArrayRelationships', () => {
  it('returns all object relationships (including manual)', () => {
    const rels = getLocalDBObjectRelationships('chinook', {
      schema: 'public',
      name: 'Album',
    })(metadata);
    expect(rels.map((r) => r.name)).toEqual([
      'album_object_fk',
      'album_object_manual',
    ]);
  });

  it('returns all array relationships (including manual)', () => {
    const rels = getLocalDBArrayRelationships('chinook', {
      schema: 'public',
      name: 'Album',
    })(metadata);
    expect(rels.map((r) => r.name)).toEqual([
      'album_array_fk',
      'album_array_manual',
    ]);
  });

  it('returns an empty array for a table with no relationships', () => {
    expect(
      getLocalDBObjectRelationships('chinook', {
        schema: 'public',
        name: 'Artist',
      })(metadata),
    ).toEqual([]);
  });
});

describe('getForeignKeyRelationships / selectForeignKeyRelationships', () => {
  it('keeps only FK-based relationships and tags them with type', () => {
    const fks = getForeignKeyRelationships(requireAlbumTable());
    expect(fks.map((r) => [r.name, r.type])).toEqual([
      ['album_object_fk', 'object'],
      ['album_array_fk', 'array'],
    ]);
    expect(fks[0].table).toEqual({ schema: 'public', name: 'Album' });
  });

  it('aggregates FK relationships across all tables of a source', () => {
    const fks = selectForeignKeyRelationships('chinook')(metadata);
    expect(fks.map((r) => r.name)).toEqual([
      'album_object_fk',
      'album_array_fk',
    ]);
  });
});

describe('getRoles / selectRoles', () => {
  it('aggregates and dedupes every referenced role', () => {
    const roles = getRoles(metadata.metadata);
    expect([...roles].sort()).toEqual(
      [
        'action_role',
        'admin',
        'allow_role',
        'editor',
        'introspect_role',
        'limit_role',
        'lm_reader',
        'rs_role',
        'user',
      ].sort(),
    );
  });

  it('returns an empty array for undefined metadata', () => {
    expect(getRoles(undefined)).toEqual([]);
  });

  it('selectRoles delegates to getRoles', () => {
    expect([...selectRoles(metadata)].sort()).toEqual(
      [...getRoles(metadata.metadata)].sort(),
    );
  });
});

describe('remote schema selectors', () => {
  it('findRemoteSchema finds by name', () => {
    expect(findRemoteSchema('my_remote')(metadata)?.name).toBe('my_remote');
  });

  it('findRemoteSchema returns undefined for unknown name', () => {
    expect(findRemoteSchema('nope')(metadata)).toBeUndefined();
  });

  it('findRemoteSchemaRelationship finds by name', () => {
    expect(findRemoteSchemaRelationship('my_remote')(metadata)?.name).toBe(
      'my_remote',
    );
  });

  it('getAllRemoteSchemaRelationships returns all remote schemas', () => {
    expect(getAllRemoteSchemaRelationships()(metadata)).toHaveLength(1);
  });
});

describe('getOperationsFromQueryCollection', () => {
  it('returns the queries of a collection', () => {
    expect(getOperationsFromQueryCollection('allowed')(metadata)).toEqual([
      { name: 'q1', query: 'query { a }' },
    ]);
  });

  it('returns an empty array for an unknown collection', () => {
    expect(getOperationsFromQueryCollection('nope')(metadata)).toEqual([]);
  });

  it('returns an empty array for undefined metadata', () => {
    expect(getOperationsFromQueryCollection('allowed')(undefined)).toEqual([]);
  });
});

describe('findMetadataSourceSchemaNames', () => {
  it('returns the unique, truthy schema names of a source', () => {
    const source = requireSource('chinook');
    expect(findMetadataSourceSchemaNames(source)).toEqual(['public']);
  });

  it('reads the dataset as the schema for bigquery tables', () => {
    const source = requireSource('bq');
    expect(findMetadataSourceSchemaNames(source)).toEqual(['analytics']);
  });
});

describe('getRemoteDatabaseRelationshipsFromSource / getRemoteDatabaseRelationships', () => {
  // NOTE: The implementation filters on `'to_remote_schema' in definition`
  // (i.e. remote-SCHEMA relationships) even though it is named/typed for
  // source-to-source (remote DATABASE) relationships. These tests pin the
  // CURRENT behavior; the discrepancy is reported in BUGS.md / the PR body.
  it('returns the relationships matching the current predicate', () => {
    const source = requireSource('chinook');
    const rels = getRemoteDatabaseRelationshipsFromSource(source, {
      schema: 'public',
      name: 'Album',
    });
    expect(rels.map((r) => r.name)).toEqual(['album_to_remote_schema']);
  });

  it('returns an empty array when the table has no matching relationships', () => {
    const source = requireSource('chinook');
    expect(
      getRemoteDatabaseRelationshipsFromSource(source, {
        schema: 'public',
        name: 'Artist',
      }),
    ).toEqual([]);
  });

  it('getRemoteDatabaseRelationships returns [] for an unknown source', () => {
    expect(
      getRemoteDatabaseRelationships({
        source: 'nope',
        schema: 'public',
        name: 'Album',
      })(metadata),
    ).toEqual([]);
  });
});

describe('event trigger selectors', () => {
  it('selectRawEventTriggers flattens all triggers across sources/tables', () => {
    expect(selectRawEventTriggers(metadata).map((et) => et.name)).toEqual([
      'album_insert_trigger',
      'album_manual_trigger',
    ]);
  });

  it('selectRawEventTriggers returns [] for undefined metadata', () => {
    expect(selectRawEventTriggers(undefined)).toEqual([]);
  });

  it('selectEventTriggers attaches table info to each trigger', () => {
    const triggers = selectEventTriggers(metadata);
    expect(triggers).toHaveLength(2);
    expect(triggers[0].table).toEqual({
      schema: 'public',
      name: 'Album',
      source: 'chinook',
    });
  });

  it('selectEventTriggerByName finds a trigger and attaches table info', () => {
    const trigger = selectEventTriggerByName('album_manual_trigger')(metadata);
    expect(trigger?.eventTrigger.name).toBe('album_manual_trigger');
    expect(trigger?.source.name).toBe('chinook');
  });

  it('selectEventTriggerByName returns undefined for unknown names', () => {
    expect(selectEventTriggerByName('nope')(metadata)).toBeUndefined();
  });

  it('selectEventsTriggersByTable returns the triggers for a table', () => {
    const triggers = selectEventsTriggersByTable({
      table: { schema: 'public', name: 'Album' },
      source: 'chinook',
    })(metadata);
    expect(triggers.map((et) => et.name)).toEqual([
      'album_insert_trigger',
      'album_manual_trigger',
    ]);
  });

  it('selectEventsTriggersByTable returns [] for an unknown source', () => {
    expect(
      selectEventsTriggersByTable({
        table: { schema: 'public', name: 'Album' },
        source: 'nope',
      })(metadata),
    ).toEqual([]);
  });

  it('selectManualEventsTriggers keeps only enable_manual triggers', () => {
    const triggers = selectManualEventsTriggers({
      table: { schema: 'public', name: 'Album' },
      source: 'chinook',
    })(metadata);
    expect(triggers.map((et) => et.name)).toEqual(['album_manual_trigger']);
  });
});

describe('event trigger selectors — tables without an event_triggers array', () => {
  // `event_triggers` is optional, and `export_metadata` omits it for a tracked
  // table that has no triggers (e.g. right after the last trigger on that table
  // is deleted). The selectors must tolerate the array being absent and must not
  // throw ("undefined is not iterable" / "cannot read 'map' of undefined").
  // These regressions establish selector behavior; they do not reproduce the
  // hosted React error or verify its uncaptured nested cause.
  const metadata: Metadata = {
    resource_version: 1,
    metadata: {
      version: 3,
      sources: [
        {
          name: 'default',
          kind: 'postgres',
          configuration: {},
          tables: [
            // a tracked table with NO `event_triggers` key at all
            { table: { schema: 'public', name: 'user_table' } },
            // a sibling table that does have a trigger, so we can prove the
            // untriggered table is skipped rather than aborting the scan
            {
              table: { schema: 'public', name: 'orders' },
              event_triggers: [
                {
                  name: 'orders_insert',
                  definition: {
                    enable_manual: false,
                    insert: { columns: '*' },
                  },
                  retry_conf: { num_retries: 0 },
                },
              ],
            },
          ],
        },
      ],
    },
  } as unknown as Metadata;

  it('selectEventTriggerByName scans past an untriggered table and finds the trigger', () => {
    expect(
      selectEventTriggerByName('orders_insert')(metadata)?.eventTrigger.name,
    ).toBe('orders_insert');
  });

  it('selectEventTriggerByName returns undefined (does not throw) for a missing trigger', () => {
    expect(selectEventTriggerByName('nope')(metadata)).toBeUndefined();
  });

  it('selectEventTriggers skips tables without an event_triggers array', () => {
    expect(selectEventTriggers(metadata).map((et) => et.name)).toEqual([
      'orders_insert',
    ]);
  });

  it('selectRawEventTriggers skips tables without an event_triggers array', () => {
    expect(selectRawEventTriggers(metadata).map((et) => et.name)).toEqual([
      'orders_insert',
    ]);
  });
});

describe('getSupportsForeignKeys', () => {
  it('is false only for bigquery', () => {
    const bq = requireSource('bq');
    expect(getSupportsForeignKeys(bq)).toBe(false);
  });

  it('is true for postgres', () => {
    const pg = requireSource('chinook');
    expect(getSupportsForeignKeys(pg)).toBe(true);
  });

  it('is true (assumed) for an undefined source', () => {
    expect(getSupportsForeignKeys(undefined)).toBe(true);
  });
});
