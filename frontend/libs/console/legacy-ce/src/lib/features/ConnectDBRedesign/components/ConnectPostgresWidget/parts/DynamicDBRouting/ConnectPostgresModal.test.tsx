import { fireEvent, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { testRenderWithClient } from '@hasura/shared/testing';
import z from 'zod';

import { ConnectPostgresModal } from './ConnectPostgresModal';
import { schema } from '../../schema';
import { generatePostgresRequestPayload } from '../../utils/generateRequests';

/**
 * Regression tests for the Dynamic DB Routing Add/Edit Connection modal.
 *
 * The modal used to bind the "Isolation Level" and "Use Prepared Statements"
 * advanced settings to the object-typed path `configuration.connectionInfo`
 * instead of the leaf fields `configuration.connectionInfo.isolationLevel` and
 * `configuration.connectionInfo.usePreparedStatements`.
 *
 * Because the Isolation Level select writes a string, binding it onto the
 * object-typed `connectionInfo` field made Zod fail with
 * "Expected object, received string", and the Use Prepared Statements toggle
 * never applied. See Zendesk #15486.
 */

const validDefaultValues: z.infer<typeof schema> = {
  name: 'chinook',
  configuration: {
    connectionInfo: {
      databaseUrl: {
        connectionType: 'databaseUrl',
        url: 'postgresql://user:password@localhost:5432/chinook',
      },
      isolationLevel: 'read-committed',
      usePreparedStatements: false,
    },
  },
};

describe('ConnectPostgresModal (Dynamic DB Routing) field binding', () => {
  const originalEnv = window.__env;
  // The modal renders the AppContext-connected WarningCard, which reads
  // `envVars.consoleMode` / the derived console type to pick a docs link, so the
  // render needs a populated env (testRenderWithClient seeds AppContext from
  // `window.__env`).
  beforeAll(() => {
    window.__env = {
      consoleMode: 'server',
      consoleType: 'oss',
    } as typeof window.__env;
    // The Radix Switch used by "Use Prepared Statements" relies on
    // ResizeObserver, which jsdom does not implement. It is instantiated with
    // `new`, so the stub must be constructable (a class, not an arrow fn).
    global.ResizeObserver = class {
      observe() {}
      unobserve() {}
      disconnect() {}
    } as unknown as typeof ResizeObserver;

    // The Radix UI Select opens its listbox using the Pointer Capture /
    // scrollIntoView DOM APIs, which jsdom does not implement; stub them so the
    // Select can be driven (otherwise clicking the trigger throws).
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
  afterAll(() => {
    window.__env = originalEnv;
  });

  it('binds Isolation Level and Use Prepared Statements to the connectionInfo leaf fields, not the object path', async () => {
    testRenderWithClient(
      <Theme>
        <ConnectPostgresModal
          onClose={vi.fn()}
          onSubmit={vi.fn()}
          defaultValues={validDefaultValues}
        />
      </Theme>,
    );

    // Isolation Level / Use Prepared Statements live inside the collapsed
    // "Advanced Settings" section, so expand it first.
    fireEvent.click(screen.getByRole('button', { name: /advanced settings/i }));

    // Isolation Level's field label is wired (htmlFor) to the connectionInfo
    // LEAF field. If it were re-bound to the `configuration.connectionInfo`
    // object path (the original bug), this would point at the object path and
    // fail. (The control itself is a Radix Select combobox, not a native
    // <select>, so we assert the field binding via the label's htmlFor.)
    const isolationLabel = (await screen.findByText('Isolation Level')).closest(
      'label',
    );
    expect(isolationLabel).toHaveAttribute(
      'for',
      'configuration.connectionInfo.isolationLevel',
    );

    // Use Prepared Statements' label and switch are wired to the connectionInfo
    // leaf field as well.
    const usePreparedStatementsLabel = screen
      .getByText('Use Prepared Statements')
      .closest('label');
    expect(usePreparedStatementsLabel).toHaveAttribute(
      'for',
      'configuration.connectionInfo.usePreparedStatements',
    );
    expect(
      screen.getByTestId('configuration.connectionInfo.usePreparedStatements'),
    ).toHaveAttribute('role', 'switch');
  });

  it('drives the Isolation Level select and Use Prepared Statements switch and submits the mapped leaf-field values (controller wiring)', async () => {
    const user = userEvent.setup({ pointerEventsCheck: 0 });
    const onSubmit = vi.fn();
    testRenderWithClient(
      <Theme>
        <ConnectPostgresModal
          onClose={vi.fn()}
          onSubmit={onSubmit}
          defaultValues={validDefaultValues}
        />
      </Theme>,
    );

    await user.click(
      screen.getByRole('button', { name: /advanced settings/i }),
    );

    // Flip Use Prepared Statements false -> true via the real Radix Switch.
    const preparedStatements = await screen.findByRole('switch');
    expect(preparedStatements).not.toBeChecked();
    await user.click(preparedStatements);
    expect(preparedStatements).toBeChecked();

    // Change Isolation Level via the real Radix Select (open trigger, pick option
    // from the portaled listbox).
    await user.click(screen.getByRole('combobox'));
    await user.click(
      await screen.findByRole('option', { name: 'serializable' }),
    );
    expect(screen.getByRole('combobox')).toHaveTextContent('serializable');

    await user.click(
      screen.getByRole('button', { name: /update connection/i }),
    );

    // The controller must map the controls onto the connectionInfo LEAF fields
    // (not the `configuration.connectionInfo` object path — the original bug),
    // and the modal must actually submit (the footer button lives inside the
    // form). Assert on the real onSubmit payload.
    await waitFor(() => expect(onSubmit).toHaveBeenCalledTimes(1));
    const submitted = onSubmit.mock.calls[0][0];
    expect(submitted.configuration.connectionInfo.usePreparedStatements).toBe(
      true,
    );
    expect(submitted.configuration.connectionInfo.isolationLevel).toBe(
      'serializable',
    );
    // Sibling leaf fields are untouched.
    expect(submitted.configuration.connectionInfo.databaseUrl.url).toBe(
      'postgresql://user:password@localhost:5432/chinook',
    );
    expect(
      screen.queryByText(/expected object, received string/i),
    ).not.toBeInTheDocument();
  });
});

describe('ConnectPostgresModal (Dynamic DB Routing) schema + payload round-trip', () => {
  it('rejects the isolation level string written onto the connectionInfo object path (original bug)', () => {
    // This is the shape react-hook-form produced when the select was bound to
    // `configuration.connectionInfo`: the object got overwritten with a string.
    const buggyValues = {
      name: 'chinook',
      configuration: {
        connectionInfo: 'read-committed',
      },
    };

    const result = schema.safeParse(buggyValues);

    expect(result.success).toBe(false);
    const messages = result.success
      ? []
      : result.error.issues.map((issue) => issue.message);
    expect(messages).toContain(
      'Invalid input: expected object, received string',
    );
  });

  it('accepts the isolation level / use prepared statements leaf fields', () => {
    const result = schema.safeParse(validDefaultValues);
    expect(result.success).toBe(true);
  });

  it('round-trips the leaf fields into connection_info.isolation_level / use_prepared_statements', () => {
    const payload = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'databaseUrl',
              url: 'postgresql://user:password@localhost:5432/chinook',
            },
            isolationLevel: 'serializable',
            usePreparedStatements: false,
          },
        },
      },
    });

    const connectionInfo = payload.details.configuration.connection_info;
    expect(connectionInfo.isolation_level).toBe('serializable');
    // The customer's exact case ("Use Prepared Statements = False") must be
    // preserved and not stripped out as a falsey value.
    expect(connectionInfo.use_prepared_statements).toBe(false);
  });

  it('preserves use_prepared_statements=true as well', () => {
    const payload = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'databaseUrl',
              url: 'postgresql://user:password@localhost:5432/chinook',
            },
            isolationLevel: 'repeatable-read',
            usePreparedStatements: true,
          },
        },
      },
    });

    const connectionInfo = payload.details.configuration.connection_info;
    expect(connectionInfo.isolation_level).toBe('repeatable-read');
    expect(connectionInfo.use_prepared_statements).toBe(true);
  });
});
