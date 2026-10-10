import { StoryObj, Meta } from '@storybook/react-webpack5';
import { http, HttpResponse, delay } from 'msw';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import MetadataStatus from './MetadataStatus';
import {
  InconsistentObject,
  InconsistentObjectFields,
} from '@hasura/shared/types';

const baseUrl = 'http://localhost:8080';

const inconsistentObjects: InconsistentObjectFields[] = [
  {
    type: 'table',
    reason: 'table "public.author" is not tracked',
    name: 'table public.author in source default',
    definition: { name: 'author', schema: 'public' },
    function_name: '',
  },
  {
    type: 'remote_schema',
    reason: 'remote schema "my-remote-schema" is inconsistent',
    name: 'remote schema my-remote-schema',
    message: 'connection to the remote schema server could not be established',
    definition: {
      name: 'my-remote-schema',
      definition: { url: 'http://remote-schema.example.com/graphql' },
    },
  },
];

/**
 * Deterministic pseudo-random generator (mulberry32) so a given count always
 * produces the same fixture, keeping Chromatic/snapshot output stable.
 */
const createRng = (seed: number) => {
  let state = seed;
  return () => {
    state |= 0;
    state = (state + 0x6d2b79f5) | 0;
    let t = Math.imul(state ^ (state >>> 15), 1 | state);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
};

const pick = <T,>(rng: () => number, items: T[]): T =>
  items[Math.floor(rng() * items.length)];

const SCHEMAS = ['public', 'analytics', 'billing', 'tenant_042', ''];
const SOURCES = ['default', 'replica', 'warehouse', 'legacy_pg'];
const ROLES = ['admin', 'user', 'anonymous', 'support-agent', ''];

const LONG_NAME =
  'table_with_an_extremely_long_and_unwieldy_identifier_that_wraps_across_multiple_lines_in_the_ui_'.repeat(
    2,
  );

/** Builds one inconsistent object, cycling through every known `type` and
 * message shape (string / structured / missing) to exercise edge cases. */
const buildInconsistentObject = (
  index: number,
  rng: () => number,
): InconsistentObjectFields => {
  const schema = pick(rng, SCHEMAS);
  const source = pick(rng, SOURCES);
  const table = { name: `entity_${index}`, schema };
  const messageVariant = index % 3;
  const message =
    messageVariant === 0
      ? undefined
      : messageVariant === 1
        ? `upstream error while resolving object #${index}: connection reset`
        : {
            message: `request failed for object #${index}`,
            request: {
              proxy: null,
              secure: rng() > 0.5,
              path: `/v1/graphql`,
              responseTimeout: '60',
              method: pick(rng, ['GET', 'POST', 'PUT'] as const),
              host: `service-${index}.internal`,
              requestVersion: '1' as const,
              redirectCount: '0' as const,
              port: '443' as const,
            },
          };

  const base = {
    reason:
      index % 11 === 0
        ? '' // empty reason edge case
        : index % 7 === 0
          ? `object #${index}: ${LONG_NAME}`
          : `object "${table.name}" in source "${source}" is inconsistent`,
    name: index % 13 === 0 ? '' : `object_${index}_in_${source}`,
    ...(message ? { message } : {}),
  };

  const typeCycle = index % 12;
  switch (typeCycle) {
    case 0:
      return {
        ...base,
        type: 'table',
        definition: table,
        function_name: '',
      };
    case 1:
      return {
        ...base,
        type: 'array_relation',
        definition: {
          name: `rel_${index}`,
          source,
          comment: '',
          table,
          using: {
            foreign_key_constraint_on: {
              column: `entity_${index}_id`,
              table: { name: `entity_${index - 1}`, schema },
            },
          },
        },
      };
    case 2:
      return {
        ...base,
        type: 'object_relation',
        definition: { name: `rel_${index}`, table },
      };
    case 3:
      return {
        ...base,
        type: 'remote_relationship',
        definition: {
          remote_schema: `remote-schema-${index}`,
          name: `remote_rel_${index}`,
          table,
        },
      };
    case 4:
      return {
        ...base,
        type: 'remote_schema',
        definition: {
          name: `remote-schema-${index}`,
          definition:
            index % 2 === 0
              ? { url: `https://remote-${index}.example.com/graphql` }
              : { url_from_env: `REMOTE_SCHEMA_URL_${index}` },
        },
      };
    case 5:
      return {
        ...base,
        type: pick(rng, [
          'select_permission',
          'insert_permission',
          'update_permission',
          'delete_permission',
        ] as const),
        definition: { role: pick(rng, ROLES), table: table.name },
      };
    case 6:
      return {
        ...base,
        type: 'event_trigger',
        definition: {
          configuration: { name: `trigger_${index}` },
          table,
        },
      };
    case 7:
      return {
        ...base,
        type: 'source',
        definition: `source_${index}`,
      };
    case 8:
      return {
        ...base,
        type: 'inherited role permission inconsistency',
        entity:
          index % 2 === 0
            ? {
                permission_type: pick(rng, [
                  'select',
                  'insert',
                  'update',
                  'delete',
                ] as const),
                source,
                table: table.name,
              }
            : { remote_schema: `remote-schema-${index}` },
      };
    case 9:
      return {
        ...base,
        type: 'action',
        definition: { name: `action_${index}`, kind: 'synchronous' },
      };
    case 10:
      return {
        ...base,
        type: 'function',
        definition: `fn_${index}`,
      };
    case 11:
    default:
      return {
        ...base,
        type: pick(rng, [
          'native_query',
          'stored_procedure',
          'logical_model',
          'computed_field',
          'function_permission',
          'remote_schema_permission',
          'remote_schema_remote_relationship',
        ] as const),
        definition: { raw: `unstructured payload for object #${index}` },
      };
  }
};

/**
 * Generates `count` inconsistent objects, periodically wrapping a batch of
 * them in the `objects` / `conflicts` / `definitions` grouping shapes that
 * `InconsistentObject` also allows, so the flattening logic in
 * MetadataStatus.tsx is exercised at scale, not just the flat `type` case.
 */
const generateInconsistentObjects = (count: number): InconsistentObject[] => {
  const rng = createRng(count);
  const result: InconsistentObject[] = [];
  let i = 0;
  let groupIndex = 0;

  while (i < count) {
    const remaining = count - i;
    const shouldGroup = remaining > 5 && groupIndex % 17 === 0 && i > 0;

    if (shouldGroup) {
      const groupSize = Math.min(1 + Math.floor(rng() * 4), remaining);
      const objects = Array.from({ length: groupSize }, (_, j) =>
        buildInconsistentObject(i + j, rng),
      );
      const wrapperKind = groupIndex % 3;
      const reason = `group of ${groupSize} conflicting objects (batch ${groupIndex})`;

      if (wrapperKind === 0) {
        result.push({ objects, reason });
      } else if (wrapperKind === 1) {
        result.push({ conflicts: objects, reason });
      } else {
        result.push({ definitions: objects, reason });
      }
      i += groupSize;
    } else {
      result.push(buildInconsistentObject(i, rng));
      i += 1;
    }
    groupIndex += 1;
  }

  return result;
};

const mockHandlers = (
  isConsistent: boolean,
  objects: InconsistentObject[] = inconsistentObjects,
) => [
  http.post(`${baseUrl}/v1/metadata`, async ({ request }) => {
    const body = (await request.json()) as { type: string };
    await delay(1);

    if (body.type === 'get_inconsistent_metadata') {
      return HttpResponse.json(
        isConsistent
          ? { inconsistent_objects: [], is_consistent: true }
          : { inconsistent_objects: objects, is_consistent: false },
      );
    }

    if (body.type === 'export_metadata') {
      return HttpResponse.json({
        metadata: { version: 3, sources: [], inherited_roles: [] },
      });
    }

    return HttpResponse.json({ message: 'success' });
  }),
];

export default {
  title: 'Features/Settings/MetadataStatus',
  component: MetadataStatus,
  parameters: {
    docs: { disable: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof MetadataStatus>;

export const Consistent: StoryObj<typeof MetadataStatus> = {
  name: '💠 Demo Consistent Metadata',
  parameters: {
    msw: mockHandlers(true),
  },
};

export const Inconsistent: StoryObj<typeof MetadataStatus> = {
  name: '💠 Demo Inconsistent Metadata',
  parameters: {
    msw: mockHandlers(false),
  },
};

export const ManyInconsistentObjects: StoryObj<typeof MetadataStatus> = {
  name: '⚡️ Perf: 100 Inconsistent Objects',
  parameters: {
    msw: mockHandlers(false, generateInconsistentObjects(100)),
  },
};

export const HugeNumberOfInconsistentObjects: StoryObj<typeof MetadataStatus> =
  {
    name: '⚡️ Perf: 1,000 Inconsistent Objects',
    parameters: {
      msw: mockHandlers(false, generateInconsistentObjects(1000)),
    },
  };

export const ExtremeNumberOfInconsistentObjects: StoryObj<
  typeof MetadataStatus
> = {
  name: '⚡️ Perf: 10,000 Inconsistent Objects',
  parameters: {
    msw: mockHandlers(false, generateInconsistentObjects(10000)),
  },
};
