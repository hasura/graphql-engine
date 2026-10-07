import React, { ReactNode } from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { Provider as ReduxProvider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
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

// The modal renders a redux- and react-query-connected WarningCard, so the
// component render needs those providers (see NeonOnboardingWizard tests).
const store = configureStore({
  reducer: {
    tables: () => ({ currentDataSource: 'postgres', dataHeaders: {} }),
  },
});

const queryClient = new QueryClient();
queryClient.setDefaultOptions({ queries: { retry: false } });

const wrapper = ({ children }: { children?: ReactNode }) => (
  <ReduxProvider store={store} key="provider">
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  </ReduxProvider>
);

describe('ConnectPostgresModal (Dynamic DB Routing) field binding', () => {
  // The Radix Switch used by "Use Prepared Statements" relies on
  // ResizeObserver, which jsdom does not implement.
  beforeAll(() => {
    global.ResizeObserver = jest.fn().mockImplementation(() => ({
      observe: jest.fn(),
      unobserve: jest.fn(),
      disconnect: jest.fn(),
    })) as unknown as typeof ResizeObserver;
  });

  it('binds Isolation Level and Use Prepared Statements to the connectionInfo leaf fields, not the object path', async () => {
    render(
      <ConnectPostgresModal
        onClose={jest.fn()}
        onSubmit={jest.fn()}
        defaultValues={validDefaultValues}
      />,
      { wrapper }
    );

    // Isolation Level / Use Prepared Statements live inside the collapsed
    // "Advanced Settings" section, so expand it first.
    fireEvent.click(screen.getByRole('button', { name: /advanced settings/i }));

    // Isolation Level is a <select> whose form field name is the connectionInfo
    // leaf field. If it were re-bound to the `configuration.connectionInfo`
    // object path (the original bug), these attribute assertions would fail.
    const isolationLevel = await screen.findByRole('combobox', {
      name: /isolation level/i,
    });
    expect(isolationLevel.tagName).toBe('SELECT');
    expect(isolationLevel).toHaveAttribute(
      'name',
      'configuration.connectionInfo.isolationLevel'
    );
    expect(isolationLevel).toHaveAttribute(
      'id',
      'configuration.connectionInfo.isolationLevel'
    );

    // Use Prepared Statements' label is wired (htmlFor) to the connectionInfo
    // leaf field as well.
    const usePreparedStatements = screen.getByText(
      (_content, element) =>
        element?.tagName.toLowerCase() === 'label' &&
        /use prepared statements/i.test(element.textContent ?? '')
    );
    expect(usePreparedStatements).toHaveAttribute(
      'for',
      'configuration.connectionInfo.usePreparedStatements'
    );
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
      : result.error.issues.map(issue => issue.message);
    expect(messages).toContain('Expected object, received string');
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
