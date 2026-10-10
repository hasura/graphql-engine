import { http, HttpResponse } from 'msw';
import { setupServer } from 'msw/node';
import { screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';

import {
  testRenderWithClient,
  createDefaultInitialData,
  metadataReducer,
  isMetadataError,
} from '@hasura/shared/testing';
import { DynamicDBRouting } from './DynamicDBRouting';

/**
 * Component integration test for the Dynamic DB Routing Add/Edit Connection
 * flow. Renders the REAL DynamicDBRouting component (which contains the fixed
 * ConnectPostgresModal), drives it via user events through the real Radix UI
 * controls, and captures the actual outgoing metadata request through an
 * in-memory (msw) HTTP layer.
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
  capturedMetadataRequests.filter((req) => req?.type === 'pg_update_source');

const server = setupServer();
const originalEnv = window.__env;

beforeAll(() => {
  // The modal's AppContext-connected WarningCard reads envVars.consoleMode and
  // the derived console type; testRenderWithClient seeds AppContext from
  // window.__env, so give it a populated server/oss env.
  window.__env = {
    consoleMode: 'server',
    consoleType: 'oss',
  } as typeof window.__env;
  server.listen({ onUnhandledRequest: 'bypass' });

  // The Radix Switch used by "Use Prepared Statements" needs ResizeObserver,
  // which jsdom does not implement. It is instantiated with `new`, so the stub
  // must be constructable (a class, not an arrow fn).
  global.ResizeObserver = class {
    observe() {}
    unobserve() {}
    disconnect() {}
  } as unknown as typeof ResizeObserver;

  // The Radix UI Select relies on the Pointer Capture / scrollIntoView DOM APIs
  // when opening its listbox; jsdom does not implement them, so without these
  // stubs clicking the trigger throws and the options never render. (This — not
  // any inherent limitation — is what made the Select "undrivable" previously.)
  if (!Element.prototype.hasPointerCapture) {
    Element.prototype.hasPointerCapture = () => false;
  }
  if (!Element.prototype.setPointerCapture) {
    Element.prototype.setPointerCapture = () => undefined;
  }
  if (!Element.prototype.releasePointerCapture) {
    Element.prototype.releasePointerCapture = () => undefined;
  }
  if (!Element.prototype.scrollIntoView) {
    Element.prototype.scrollIntoView = () => undefined;
  }
});
afterEach(() => server.resetHandlers());
afterAll(() => {
  server.close();
  window.__env = originalEnv;
});

beforeEach(() => {
  capturedMetadataRequests.length = 0;
  currentMetadata = buildSeededMetadata();

  server.use(
    http.get('*/v1alpha1/config', () => HttpResponse.json({})),
    http.post('*/v1/metadata', async ({ request }) => {
      const body = (await request.json()) as any;
      capturedMetadataRequests.push(body);
      const response = metadataReducer(currentMetadata, body);
      if (isMetadataError(response)) {
        return HttpResponse.json(response.error, { status: response.status });
      }
      currentMetadata = response;
      return HttpResponse.json(response);
    }),
  );
});

type User = ReturnType<typeof userEvent.setup>;

// Advanced settings (isolation level / prepared statements) live inside a
// collapsible that is closed by default.
const openAdvancedSettings = async (user: User) => {
  await user.click(screen.getByRole('button', { name: /advanced settings/i }));
};

// Drive the Radix UI Select (Isolation Level): open the trigger, then pick the
// option Radix renders in a portal. (The trigger has no accessible name wired,
// and there is a single combobox in the modal, so we match it by role.)
const selectIsolationLevel = async (user: User, value: string) => {
  await user.click(screen.getByRole('combobox'));
  await user.click(await screen.findByRole('option', { name: value }));
};

const getDialog = () => screen.getByRole('dialog');

describe('DynamicDBRouting connection modal - Use Prepared Statements / Isolation Level', () => {
  it('edits an existing connection: reads stored values, toggles Use Prepared Statements true -> false, changes isolation, and persists it (customer scenario)', async () => {
    const user = userEvent.setup({ pointerEventsCheck: 0 });
    testRenderWithClient(
      <Theme>
        <DynamicDBRouting sourceName="default" />
      </Theme>,
    );

    // Wait for the seeded connection-set member to be listed.
    const editButton = await screen.findByRole(
      'button',
      { name: /edit connection/i },
      { timeout: 5000 },
    );
    await user.click(editButton);
    await openAdvancedSettings(user);

    // Read round-trip: stored use_prepared_statements=true / serializable, via
    // the real adaptResponse read path.
    const preparedStatements = await screen.findByRole('switch');
    expect(preparedStatements).toBeChecked();
    expect(screen.getByRole('combobox')).toHaveTextContent('serializable');
    expect(
      screen.getByDisplayValue(
        'postgresql://user:password@localhost:5432/tenant1',
      ),
    ).toBeInTheDocument();

    // Customer action: turn Use Prepared Statements OFF and change isolation.
    await user.click(preparedStatements);
    expect(preparedStatements).not.toBeChecked();
    await selectIsolationLevel(user, 'read-committed');
    expect(screen.getByRole('combobox')).toHaveTextContent('read-committed');

    await user.click(
      within(getDialog()).getByRole('button', { name: /update connection/i }),
    );

    // The original bug surfaced as this validation error on submit.
    expect(
      screen.queryByText(/expected object, received string/i),
    ).not.toBeInTheDocument();

    // The actual outgoing metadata request must carry the new values in the
    // correct connection_set member (not just local component state).
    await waitFor(() => expect(pgUpdateSourceRequests()).toHaveLength(1));
    const req = pgUpdateSourceRequests()[0];
    const member = req.args.configuration.connection_set.find(
      (c: any) => c.name === 'tenant-1',
    );
    expect(member.connection_info.use_prepared_statements).toBe(false);
    expect(member.connection_info.isolation_level).toBe('read-committed');
    // Sibling fields must not be corrupted.
    expect(member.connection_info.database_url).toBe(
      'postgresql://user:password@localhost:5432/tenant1',
    );

    // Reopen the connection: the persisted false must round-trip back into the
    // form (mock persistence + real adaptResponse read path).
    const reopenButton = await screen.findByRole('button', {
      name: /edit connection/i,
    });
    await user.click(reopenButton);
    await openAdvancedSettings(user);
    await waitFor(() => expect(screen.getByRole('switch')).not.toBeChecked());
    expect(screen.getByRole('combobox')).toHaveTextContent('read-committed');
  }, 30000);

  it('adds a new connection with Use Prepared Statements / Isolation Level set, preserving the existing member, without a validation error', async () => {
    const user = userEvent.setup({ pointerEventsCheck: 0 });
    testRenderWithClient(
      <Theme>
        <DynamicDBRouting sourceName="default" />
      </Theme>,
    );

    const addButton = await screen.findByRole(
      'button',
      { name: /add connection/i },
      { timeout: 5000 },
    );
    await user.click(addButton);

    const dialog = getDialog();
    await user.type(
      await within(dialog).findByPlaceholderText('Connection name'),
      'tenant-2',
    );
    await user.type(
      within(dialog).getByPlaceholderText(
        'postgresql://username:password@hostname:port/postgres',
      ),
      'postgresql://user:password@localhost:5432/tenant2',
    );

    await openAdvancedSettings(user);
    await selectIsolationLevel(user, 'repeatable-read');
    // undefined -> true
    await user.click(within(dialog).getByRole('switch'));

    // The modal footer's submit button shares its label ("Add Connection") with
    // the list button, so scope it to the dialog.
    await user.click(
      within(dialog).getByRole('button', { name: /add connection/i }),
    );

    expect(
      screen.queryByText(/expected object, received string/i),
    ).not.toBeInTheDocument();

    await waitFor(() => expect(pgUpdateSourceRequests()).toHaveLength(1));
    const req = pgUpdateSourceRequests()[0];
    const connectionSet = req.args.configuration.connection_set;
    // Existing member preserved + new member added.
    expect(connectionSet).toHaveLength(2);
    expect(connectionSet.find((c: any) => c.name === 'tenant-1')).toBeDefined();
    const added = connectionSet.find((c: any) => c.name === 'tenant-2');
    expect(added.connection_info.use_prepared_statements).toBe(true);
    expect(added.connection_info.isolation_level).toBe('repeatable-read');
  }, 30000);
});
