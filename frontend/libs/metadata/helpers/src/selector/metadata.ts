import { extractTableInfo } from '@hasura/shared/utils';
import { adaptFunction, areTablesEqual } from '@hasura/metadata/helpers';
import {
  type Metadata,
  type Source,
  type Table,
  type EventTrigger,
  type DataTarget,
  type SourceToSourceRelationship,
  type MetadataTable,
  isObjectFkRelationship,
  isArrayFkRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  SameTableObjectRelationship,
  TableFunction,
} from '@hasura/shared/types';

/*

How do I implement my custom selector apart from the ones provided here?

In your local component, write the selector as per your requirement ->

// use the useMetadata from lib
import {useMetadata} from '@hasura/metadata/api`;

// This will be your selector
const getRemoteSchema = (remoteSchemaName: string) => (m: Metadata) => m.metadata.remote_schemas?.find(s => s.name === remoteSchemaName)

// usage
const { isLoading, data: remote_schemas } = useMetadata(getRemoteSchema('foobar'));

*/

export const getSources = () => (m: Metadata | undefined) =>
  m?.metadata.sources ?? [];

export const findSource =
  (dataSourceName: string | undefined) => (m: Metadata | undefined) =>
    dataSourceName ? findMetadataSource(dataSourceName, m) : undefined;

export const findNativeQuery =
  (dataSourceName: string, nativeQueryName: string) => (m: Metadata) =>
    findMetadataSource(dataSourceName, m)?.native_queries?.find(
      (nq) => nq.root_field_name === nativeQueryName,
    );

export const findLogicalModel =
  (dataSourceName: string, logicalModelName: string) => (m: Metadata) =>
    findMetadataSource(dataSourceName, m)?.logical_models?.find(
      (lm) => lm.name === logicalModelName,
    );

export const getTables = (dataSourceName: string) => (m: Metadata) =>
  findMetadataSource(dataSourceName, m)?.tables ?? [];

export const selectFunctions =
  (dataSourceName: string, schemaName?: string) => (m: Metadata) => {
    const functions = findMetadataSource(dataSourceName, m)?.functions ?? [];

    return schemaName
      ? functions.filter(
          (fn) => adaptFunction(fn.function).schema === schemaName,
        )
      : functions;
  };

export const findTable =
  (dataSourceName: string, table: Table) => (m: Metadata | undefined) =>
    findMetadataTable(dataSourceName, table, m);

export const findFunction =
  (dataSourceName: string, qualifiedFunction: TableFunction) => (m: Metadata) =>
    findMetadataFunction(dataSourceName, qualifiedFunction, m);

export const resourceVersion = () => (m: Metadata) => m?.resource_version;

export const isCollectionInAllowlist =
  (collectionName: string) =>
  (m: Metadata): boolean => {
    return (
      m.metadata?.allowlist?.find(
        (entry) => entry?.collection === collectionName,
      ) !== undefined
    );
  };

export const getLocalDBObjectRelationships =
  (currentDataSource: string, table: Table) => (m: Metadata) => {
    const metadataTable = findTable(currentDataSource, table)(m);
    return metadataTable?.object_relationships ?? [];
  };

export const getNewRolePermission =
  (queryCollectionName: string) => (m: Metadata) => {
    const queryCollectionDefinition = m.metadata?.allowlist?.find(
      (qs) => qs.collection === queryCollectionName,
    );
    return queryCollectionDefinition?.scope?.global === false
      ? queryCollectionDefinition?.scope?.roles
      : [];
  };

type allowedRelationshipTypes = 'object' | 'array';

export const getForeignKeyRelationships = (
  metadataTable: MetadataTable,
): ({
  table: Table;
  type: allowedRelationshipTypes;
} & (
  | LocalTableArrayRelationship
  | LocalTableObjectRelationship
  | SameTableObjectRelationship
))[] => {
  const filteredObjectRelationships = (metadataTable.object_relationships ?? [])
    .filter(isObjectFkRelationship)
    .map((rel) => ({
      ...rel,
      table: metadataTable.table,
      type: 'object' as allowedRelationshipTypes,
    }));

  const filteredArrayRelationships = (metadataTable.array_relationships ?? [])
    .filter(isArrayFkRelationship)
    .map((rel) => ({
      ...rel,
      table: metadataTable.table,
      type: 'array' as allowedRelationshipTypes,
    }));

  return [...filteredObjectRelationships, ...filteredArrayRelationships];
};

export const selectForeignKeyRelationships =
  (dataSourceName: string) => (m: Metadata) => {
    const source = findMetadataSource(dataSourceName, m);
    const tables = source?.tables ?? [];
    const foreignKeyRelationships = tables
      .map((t) => getForeignKeyRelationships(t))
      .flat();
    return foreignKeyRelationships;
  };

export const selectRoles = (meta: Metadata) => getRoles(meta.metadata);

export const getRoles = (metadata: Metadata['metadata'] | undefined) => {
  const roles: string[] = [];
  if (!metadata) {
    return roles;
  }

  /**
   * Actions Permissions
   */
  metadata.actions?.forEach((action) =>
    action.permissions?.forEach((p) => roles.push(p.role)),
  );

  /**
   * Table Permissions
   */
  metadata.sources.forEach((source) => {
    source.tables.forEach((table) => {
      table.select_permissions?.forEach((permission) =>
        roles.push(permission.role),
      );

      table.insert_permissions?.forEach((permission) =>
        roles.push(permission.role),
      );

      table.update_permissions?.forEach((permission) =>
        roles.push(permission.role),
      );

      table.delete_permissions?.forEach((permission) =>
        roles.push(permission.role),
      );
    });
  });

  /**
   * Remote Schema Permissions
   */
  metadata.remote_schemas?.forEach((remoteSchema) => {
    remoteSchema?.permissions?.forEach((p) => roles.push(p.role));
  });

  /**
   * Allow List
   */
  metadata.allowlist?.forEach((al) => {
    if (al?.scope?.global === false) {
      al?.scope?.roles?.forEach((role) => roles.push(role));
    }
  });

  /**
   * API limits
   */
  Object.entries(metadata.api_limits ?? {}).forEach(([limit, value]) => {
    if (limit !== 'disabled' && typeof value !== 'boolean') {
      Object.keys(value?.per_role ?? {}).forEach((role) => roles.push(role));
    }
  });

  /**
   * GraphQL introspection limits
   */
  metadata?.graphql_schema_introspection?.disabled_for_roles.forEach((role) =>
    roles.push(role),
  );

  /**
   * Logical Model Permissions
   */
  metadata.sources.forEach((source) => {
    source.logical_models?.forEach((logicalModel) => {
      logicalModel.select_permissions?.forEach((permission) =>
        roles.push(permission.role),
      );
    });
  });

  return Array.from(new Set(roles));
};

export const findRemoteSchema = (schemaName: string) => (m: Metadata) => {
  return m?.metadata.remote_schemas?.find((s) => s.name === schemaName);
};

export const getOperationsFromQueryCollection =
  (queryCollectionName: string) => (m: Metadata | undefined) => {
    const queryCollectionDefinition = m?.metadata?.query_collections?.find(
      (qs) => qs.name === queryCollectionName,
    );
    return queryCollectionDefinition?.definition?.queries ?? [];
  };

export const getRemoteDatabaseRelationships =
  (target: DataTarget) => (m: Metadata) => {
    const source = findSource(target.source)(m);

    return source
      ? getRemoteDatabaseRelationshipsFromSource(source, target)
      : [];
  };

export const findMetadataSourceSchemaNames = (source: Source): string[] => {
  return [
    ...new Set(
      source.tables
        .map((t) => extractTableInfo(t.table)?.schema)
        .filter((schema): schema is string => Boolean(schema)),
    ),
  ];
};

export const getRemoteDatabaseRelationshipsFromSource = (
  source: Source,
  target: Table,
) => {
  return (
    (source.tables
      .find((t) => areTablesEqual(t.table, target))
      ?.remote_relationships?.filter(
        (field) => 'to_remote_schema' in field.definition,
      ) as SourceToSourceRelationship[]) ?? []
  );
};

export const getAllRemoteSchemaRelationships = () => (m: Metadata) => {
  return m.metadata?.remote_schemas ?? [];
};

export const findRemoteSchemaRelationship =
  (sourceRemoteSchema: string) => (m: Metadata) => {
    return m?.metadata?.remote_schemas?.find(
      (rs) => rs.name === sourceRemoteSchema,
    );
  };

export const getLocalDBArrayRelationships =
  (currentDataSource: string, table: Table) => (m: Metadata) => {
    const metadataTable = findTable(currentDataSource, table)(m);
    return metadataTable?.array_relationships ?? [];
  };

export type EventTriggerWithTableInfo = EventTrigger & {
  table: {
    name: string;
    schema: string;
    source: string;
  };
};

const mapEventTrigger = (
  et: EventTrigger,
  table: Table,
  sourceName: string,
) => {
  return {
    ...et,
    table: {
      ...extractTableInfo(table)!,
      source: sourceName,
    },
  };
};

export const selectRawEventTriggers = (
  m: Metadata | undefined,
): EventTrigger[] => {
  return (
    m?.metadata?.sources?.flatMap((source) =>
      // `event_triggers` is optional and omitted by export_metadata for tables
      // with none, so guard against the absent array.
      source.tables.flatMap((t) => t.event_triggers ?? []),
    ) ?? []
  );
};

export const selectEventTriggers = (
  m: Metadata | undefined,
): EventTriggerWithTableInfo[] => {
  return (
    m?.metadata?.sources?.flatMap((source) =>
      source.tables.flatMap((t) =>
        (t.event_triggers ?? []).map((et) =>
          mapEventTrigger(et, t.table, source.name),
        ),
      ),
    ) ?? []
  );
};

export const selectEventTriggerByName =
  (name: string) => (m: Metadata | undefined) => {
    if (!m?.metadata.sources.length) {
      return undefined;
    }

    for (const source of m?.metadata.sources) {
      for (const t of source.tables) {
        // `event_triggers` is optional and omitted by export_metadata for
        // tables with none (e.g. after the last trigger is deleted); guard so
        // this scan does not throw "undefined is not iterable".
        for (const et of t.event_triggers ?? []) {
          if (et.name === name) {
            return {
              source,
              eventTrigger: et,
              table: t.table,
            };
          }
        }
      }
    }

    return undefined;
  };

type SelectEventTriggersByTableArgs = {
  table: Table;
  source: string;
};

export const selectEventsTriggersByTable =
  ({ table, source }: SelectEventTriggersByTableArgs) =>
  (m: Omit<Metadata, 'resource_version'>) => {
    const tables = m.metadata.sources.find((s) => s.name === source)?.tables;
    if (!tables?.length) {
      return [];
    }

    const foundTable = tables.find((t) => areTablesEqual(t.table, table));

    return foundTable?.event_triggers ?? [];
  };

export const selectManualEventsTriggers =
  (args: SelectEventTriggersByTableArgs) =>
  (m: Omit<Metadata, 'resource_version'>) => {
    const eventTriggers = selectEventsTriggersByTable(args)(m);
    return eventTriggers.filter((et) => et?.definition?.enable_manual);
  };

/*
  These utility functions are meant to be the lowest level of re - usable metadata utility operations.

  The flow of lowest level to higher level is:

    utility functions -> selectors -> useMetadata hook

  These functions are separated from the selectors so that a developer may make use of them within a custom selector.

  For example, if you are composing a custom selector to get table metadata comments,
  you can leverage the findMetadataTable() utility function alongside useMetadata() like this:

const { isLoading, data: savedComment } = useMetadata(
  m => MetadataUtils.findMetadataTable(dataSourceName, table, m)?.configuration?.comment
);

  These utility functions are also used within selectors.ts

  Add utility functions only if it would be helpful for common / universally needed operations.

*/

export const findMetadataSource = (
  dataSourceName: string,
  m: Metadata | undefined,
) => m?.metadata.sources.find((s) => s.name === dataSourceName);

export const findMetadataTable = (
  dataSourceName: string,
  table: Table,
  m: Metadata | undefined,
) =>
  findMetadataSource(dataSourceName, m)?.tables.find((t) =>
    areTablesEqual(t.table, table),
  );

export const findMetadataFunction = (
  dataSourceName: string,
  qualifiedFunction: TableFunction,
  m: Metadata,
) =>
  findMetadataSource(dataSourceName, m)?.functions?.find((fn) =>
    areTablesEqual(fn.function, qualifiedFunction),
  );

export const getSupportsForeignKeys = (source: Source | undefined) =>
  source?.kind !== 'bigquery';
