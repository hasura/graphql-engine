import React from 'react';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { fireEvent, screen, waitFor } from '@testing-library/react';

import { renderWithClient } from '../../../../../../hooks/__tests__/common/decorator';
import { createDefaultInitialData } from '../../../../../../mocks/metadata.mock';
import { metadataReducer } from '../../../../../../mocks/actions';
import { DynamicDBRouting } from './DynamicDBRouting';

/**
 * Component integration test for the Dynamic DB Routing Add/Edit Connection
 * flow. Renders the REAL DynamicDBRouting component (which contains the fixed
 * ConnectPostgresModal), drives it via user events, and captures the actual
 * outgoing metadata request through an in-memory (msw) HTTP layer.
 *
 * This exercises the real Zod schema, the real generateRequests serializer and
 * the real adaptResponse deserializer. It runs in jsdom (not a real browser)
 * and persistence is simulated by the msw metadata reducer.
 *
 * Regression target: Zendesk #15486 / PR #11721 — setting Use Prepared
 * Statements / Isolation Level on a dynamic routing connection used to fail
 * Zod validation with "Expected object, received string".
 */

const capturedMetadataRequests: any[] = [];
let currentMetadata: any;

const buildSeededMetadata = () => {
  const seed: any = createDefaultInitialData();
  // The default mock data already contains a postgres source named "default".
  // Add a dynamic-routing connection template + a connection-set member to it.
  const source = seed.metadata.sources.find((s: any) => s.name === 'default');
  source.configuration.connection_template = { template: '{{$.default}}' };
  source.configuration.connection_set = [
    {
      name: 'tenant-1',
      connection_info: {
        database_url: 'postgresql://user:password@localhost:5432/tenant1',
        use_prepared_statements: true,
        isolation_level: 'serializable',
      },
    },
  ];
  return seed;
};

const pgUpdateSourceRequests = () =>
  capturedMetadataRequests.filter(req => req?.type === 'pg_update_source');

const server = setupServer();

beforeAll(() => {
  server.listen({ onUnhandledRequest: 'bypass' });
  // The Radix Switch used by "Use Prepared Statements" needs ResizeObserver,
  // which jsdom does not implement.
  global.ResizeObserver = jest.fn().mockImplementation(() => ({
    observe: jest.fn(),
    unobserve: jest.fn(),
    disconnect: jest.fn(),
  })) as unknown as typeof ResizeObserver;
});
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

beforeEach(() => {
  capturedMetadataRequests.length = 0;
  currentMetadata = buildSeededMetadata();

  server.use(
    rest.get('/v1alpha1/config', (_req, res, ctx) => res(ctx.json({}))),
    rest.post('/v1/metadata', async (req, res, ctx) => {
      const body = (await req.json()) as any;
      capturedMetadataRequests.push(body);
      const response = metadataReducer(currentMetadata, body) as any;
      if ('error' in response && response.error) {
        return res(ctx.status(response.status), ctx.json(response.error));
      }
      if ('metadata' in response) {
        currentMetadata = response;
      }
      return res(ctx.json(response));
    })
  );
});

const openAdvancedSettings = () => {
  fireEvent.click(screen.getByRole('button', { name: /advanced settings/i }));
};

describe('DynamicDBRouting connection modal - Use Prepared Statements / Isolation Level', () => {
  it('edits an existing connection: reads the stored values, toggles Use Prepared Statements true -> false, and persists it (customer scenario)', async () => {
    renderWithClient(<DynamicDBRouting sourceName="default" />);

    // Wait for the seeded connection-set member to be listed.
    const editButton = await screen.findByRole(
      'button',
      { name: /edit connection/i },
      { timeout: 5000 }
    );
    fireEvent.click(editButton);

    // Modal opens with the stored connection info adapted back into the form.
    openAdvancedSettings();

    const preparedStatements = await screen.findByRole('switch');
    const isolationLevel = screen.getByRole('combobox', {
      name: /isolation level/i,
    });

    // Read round-trip: stored use_prepared_statements=true / serializable.
    expect(preparedStatements).toBeChecked();
    expect(isolationLevel).toHaveValue('serializable');

    // Customer action: turn Use Prepared Statements OFF and change isolation.
    fireEvent.click(preparedStatements);
    expect(preparedStatements).not.toBeChecked();
    fireEvent.change(isolationLevel, { target: { value: 'read-committed' } });

    fireEvent.click(screen.getByRole('button', { name: /update connection/i }));

    // The original bug surfaced as this validation error on submit.
    expect(
      screen.queryByText(/expected object, received string/i)
    ).not.toBeInTheDocument();

    // The actual outgoing metadata request must carry the new values in the
    // correct connection_set member (not just local component state).
    await waitFor(() => expect(pgUpdateSourceRequests()).toHaveLength(1));
    const req = pgUpdateSourceRequests()[0];
    const member = req.args.configuration.connection_set.find(
      (c: any) => c.name === 'tenant-1'
    );
    expect(member.connection_info.use_prepared_statements).toBe(false);
    expect(member.connection_info.isolation_level).toBe('read-committed');
    // Sibling fields must not be corrupted.
    expect(member.connection_info.database_url).toBe(
      'postgresql://user:password@localhost:5432/tenant1'
    );

    // Reopen the connection: the persisted false must round-trip back into the
    // form (mock persistence + real adaptResponse read path).
    await waitFor(() =>
      expect(
        screen.getByRole('button', { name: /edit connection/i })
      ).toBeInTheDocument()
    );
    fireEvent.click(screen.getByRole('button', { name: /edit connection/i }));
    openAdvancedSettings();
    await waitFor(() => expect(screen.getByRole('switch')).not.toBeChecked());
    expect(
      screen.getByRole('combobox', { name: /isolation level/i })
    ).toHaveValue('read-committed');
  }, 20000);

  it('adds a new connection with Use Prepared Statements / Isolation Level set, without a validation error', async () => {
    renderWithClient(<DynamicDBRouting sourceName="default" />);

    const addButton = await screen.findByRole(
      'button',
      { name: /add connection/i },
      { timeout: 5000 }
    );
    fireEvent.click(addButton);

    fireEvent.change(screen.getByTestId('name'), {
      target: { value: 'tenant-2' },
    });
    fireEvent.change(
      screen.getByTestId('configuration.connectionInfo.databaseUrl.url'),
      {
        target: {
          value: 'postgresql://user:password@localhost:5432/tenant2',
        },
      }
    );

    openAdvancedSettings();
    fireEvent.change(
      screen.getByRole('combobox', { name: /isolation level/i }),
      { target: { value: 'repeatable-read' } }
    );
    // undefined -> true
    fireEvent.click(screen.getByRole('switch'));

    fireEvent.click(
      screen
        .getAllByRole('button', { name: /add connection/i })
        .pop() as HTMLElement
    );

    expect(
      screen.queryByText(/expected object, received string/i)
    ).not.toBeInTheDocument();

    await waitFor(() => expect(pgUpdateSourceRequests()).toHaveLength(1));
    const req = pgUpdateSourceRequests()[0];
    const connectionSet = req.args.configuration.connection_set;
    // Existing member preserved + new member added.
    expect(connectionSet).toHaveLength(2);
    const added = connectionSet.find((c: any) => c.name === 'tenant-2');
    expect(added.connection_info.use_prepared_statements).toBe(true);
    expect(added.connection_info.isolation_level).toBe('repeatable-read');
  }, 20000);
});
