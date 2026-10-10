import { setupServer } from 'msw/node';
import type { Metadata } from '@hasura/shared/types';
import {
  createDefaultInitialData,
  handlers,
  METADATA_WRITE_SUCCESS,
} from './metadata';
import { isMetadataError, metadataReducer } from './reducer';

const server = setupServer();

beforeAll(() => server.listen({ onUnhandledFrame: 'error' }));
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

const postMetadata = (body: Record<string, any>) =>
  fetch('http://localhost/v1/metadata', {
    method: 'POST',
    body: JSON.stringify(body),
  });

describe('metadataReducer', () => {
  it('returns the current state unchanged for export_metadata', () => {
    const state = createDefaultInitialData();
    expect(metadataReducer(state, { type: 'export_metadata', args: {} })).toBe(
      state,
    );
  });

  it('succeeds and leaves metadata unchanged for an unknown type', () => {
    const state = createDefaultInitialData();
    const result = metadataReducer(state, {
      type: 'pg_create_remote_relationship',
      args: {},
    });
    expect(isMetadataError(result)).toBe(false);
    expect(result).toEqual(state);
  });

  it('applies a known domain handler', () => {
    const state = createDefaultInitialData();
    const result = metadataReducer(state, {
      type: 'add_collection_to_allowlist',
      args: { collection: 'brand_new_collection' },
    });
    expect(isMetadataError(result)).toBe(false);
    const metadata = result as Metadata;
    expect(
      metadata.metadata.allowlist?.some(
        (c) => c.collection === 'brand_new_collection',
      ),
    ).toBe(true);
  });

  it('returns the domain error shape on invalid input', () => {
    const state = createDefaultInitialData();
    const result = metadataReducer(state, {
      type: 'add_collection_to_allowlist',
      args: { collection: 'allowed-queries' },
    });
    expect(isMetadataError(result)).toBe(true);
    if (isMetadataError(result)) {
      expect(result.status).toBe(400);
      expect(result.error).toMatchObject({
        code: 'already-exists',
        path: '$.args.collection',
      });
    }
  });

  it('threads state through bulk args and short-circuits on the first error', () => {
    const state = createDefaultInitialData();
    const result = metadataReducer(state, {
      type: 'bulk',
      args: [
        { type: 'create_query_collection', args: { name: 'bulk_collection' } },
        // duplicate -> error, so the second create must not be applied
        { type: 'create_query_collection', args: { name: 'bulk_collection' } },
        { type: 'create_query_collection', args: { name: 'never_created' } },
      ],
    });
    expect(isMetadataError(result)).toBe(true);
  });

  it('applies every arg of a successful bulk', () => {
    const state = createDefaultInitialData();
    const result = metadataReducer(state, {
      type: 'bulk',
      args: [
        { type: 'create_query_collection', args: { name: 'c1' } },
        { type: 'create_query_collection', args: { name: 'c2' } },
      ],
    });
    expect(isMetadataError(result)).toBe(false);
    const metadata = result as Metadata;
    const names = (metadata.metadata.query_collections || []).map(
      (c) => c.name,
    );
    expect(names).toEqual(expect.arrayContaining(['c1', 'c2']));
  });
});

describe('handlers', () => {
  it('serves the config endpoint', async () => {
    server.use(...handlers({ config: { is_allow_list_enabled: false } }));
    const res = await fetch('http://localhost/v1alpha1/config');
    expect(res.status).toBe(200);
    expect(await res.json()).toEqual({ is_allow_list_enabled: false });
  });

  it('export_metadata returns the current document with its resource_version', async () => {
    server.use(...handlers());
    const res = await postMetadata({ type: 'export_metadata', args: {} });
    const body = (await res.json()) as Metadata;
    expect(res.status).toBe(200);
    expect(body.resource_version).toBe(1);
    expect(body.metadata.version).toBe(3);
  });

  it('bumps resource_version and returns a success body on a successful write', async () => {
    server.use(...handlers());

    const writeRes = await postMetadata({
      type: 'pg_create_remote_relationship',
      args: {},
    });
    expect(writeRes.status).toBe(200);
    expect(await writeRes.json()).toEqual(METADATA_WRITE_SUCCESS);

    const exportRes = await postMetadata({ type: 'export_metadata', args: {} });
    const body = (await exportRes.json()) as Metadata;
    expect(body.resource_version).toBe(2);
  });

  it('returns the reducer error status and body without bumping the version', async () => {
    server.use(...handlers());

    const res = await postMetadata({
      type: 'add_collection_to_allowlist',
      args: { collection: 'allowed-queries' },
    });
    expect(res.status).toBe(400);
    expect(await res.json()).toMatchObject({ code: 'already-exists' });

    const exportRes = await postMetadata({ type: 'export_metadata', args: {} });
    const body = (await exportRes.json()) as Metadata;
    expect(body.resource_version).toBe(1);
  });

  it('isolates state between separate handlers() instances', async () => {
    server.use(...handlers());
    await postMetadata({ type: 'pg_create_remote_relationship', args: {} });

    // A fresh set of handlers starts from a clean clone.
    server.resetHandlers();
    server.use(...handlers());
    const res = await postMetadata({ type: 'export_metadata', args: {} });
    const body = (await res.json()) as Metadata;
    expect(body.resource_version).toBe(1);
  });

  it('serves the CLI-mode metadata export endpoint', async () => {
    server.use(...handlers());
    const res = await fetch('http://localhost:9693/apis/metadata?export=true');
    const body = (await res.json()) as Metadata;
    expect(res.status).toBe(200);
    expect(body.resource_version).toBe(1);
  });
});
