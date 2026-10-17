import { act, renderHook, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router';
import { AppContext, defaultAppState } from '@hasura/shared/context';
import { AppTheme, AlertProvider } from '@hasura/shared/ui';
import {
  useDatabaseMigration,
  TDatabaseMigration,
} from './useDatabaseMigration';

// Integration tests for the CLI-server "continue anyway" gate. In CLI mode the
// hook checks the CLI server status; when it is unreachable it must show an
// in-app typed-"CONFIRM" prompt (replacing the old native `prompt`) and must not
// run the migration unless the user explicitly confirms.

const getMigrationStatus = vitest.fn();
const runSQLMigrate = vitest.fn();
const runSQLBulk = vitest.fn();

vitest.mock('../../api', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return {
    ...actual,
    getMigrationStatus: (...args: unknown[]) => getMigrationStatus(...args),
    runSQLMigrate: (...args: unknown[]) => runSQLMigrate(...args),
    runSQLBulk: (...args: unknown[]) => runSQLBulk(...args),
  };
});

vitest.mock('@hasura/shared/hooks', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return {
    ...actual,
    useAuthFetchJson: () => vitest.fn(),
  };
});

const wrapper = ({ children }: { children?: React.ReactNode }) => (
  <QueryClientProvider
    client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}
  >
    <MemoryRouter>
      <AppContext.Provider
        value={
          {
            ...defaultAppState,
            envVars: { consoleMode: 'cli' },
            endpoints: {
              hasuraCliServerMigrateSettings: 'http://localhost/settings',
              hasuraCliServerMigrate: 'http://localhost/migrate',
              queryV2: 'http://localhost/v2/query',
            },
          } as any
        }
      >
        <AppTheme>
          <AlertProvider>{children}</AlertProvider>
        </AppTheme>
      </AppContext.Provider>
    </MemoryRouter>
  </QueryClientProvider>
);

const migrationArgs: TDatabaseMigration = {
  name: 'my_migration',
  up: [],
  down: [],
  source: { name: 'default' } as any,
};

const setupUser = () => userEvent.setup({ pointerEventsCheck: 0 });

const CLI_CONFIRM_MESSAGE = /CLI server is unable to access/i;

describe('useDatabaseMigration CLI-server gate', () => {
  beforeEach(() => {
    getMigrationStatus.mockReset();
    runSQLMigrate.mockReset().mockResolvedValue([]);
    runSQLBulk.mockReset().mockResolvedValue([]);
    vitest.spyOn(console, 'error').mockImplementation(() => null);
  });
  afterEach(() => {
    vitest.spyOn(console, 'error').mockRestore();
  });

  it('runs the migration without prompting when the CLI server is healthy', async () => {
    getMigrationStatus.mockResolvedValue('healthy');

    const { result } = renderHook(() => useDatabaseMigration(), { wrapper });

    await act(async () => {
      await result.current.mutateAsync(migrationArgs);
    });

    expect(getMigrationStatus).toHaveBeenCalledTimes(1);
    expect(runSQLMigrate).toHaveBeenCalledTimes(1);
    expect(screen.queryByText(CLI_CONFIRM_MESSAGE)).not.toBeInTheDocument();
  });

  it('does NOT run the migration when the unhealthy-server prompt is cancelled', async () => {
    getMigrationStatus.mockResolvedValue('unhealthy');
    const user = setupUser();

    const { result } = renderHook(() => useDatabaseMigration(), { wrapper });

    let pending: Promise<unknown>;
    act(() => {
      pending = result.current.mutateAsync(migrationArgs);
    });

    await screen.findByText(CLI_CONFIRM_MESSAGE);
    await user.click(screen.getByText('Cancel'));

    await act(async () => {
      await expect(pending).resolves.toBeNull();
    });

    expect(runSQLMigrate).not.toHaveBeenCalled();
  });

  it('runs the migration exactly once after the user types CONFIRM', async () => {
    getMigrationStatus.mockResolvedValue('unhealthy');
    const user = setupUser();

    const { result } = renderHook(() => useDatabaseMigration(), { wrapper });

    let pending: Promise<unknown>;
    act(() => {
      pending = result.current.mutateAsync(migrationArgs);
    });

    await screen.findByText(CLI_CONFIRM_MESSAGE);
    await user.type(await screen.findByRole('textbox'), 'CONFIRM');
    await user.click(screen.getByTestId('alert-confirm-button'));

    await act(async () => {
      await pending;
    });

    await waitFor(() => expect(runSQLMigrate).toHaveBeenCalledTimes(1));
  });
});
