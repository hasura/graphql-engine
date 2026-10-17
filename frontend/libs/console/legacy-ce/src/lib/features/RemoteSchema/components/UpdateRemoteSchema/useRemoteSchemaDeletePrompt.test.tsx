import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router';
import { setupServer } from 'msw/node';
import { http } from 'msw';
import {
  AppContext,
  defaultAppState,
  getEndpoints,
} from '@hasura/shared/context';
import { AppTheme, AlertProvider } from '@hasura/shared/ui';
import { handlers } from '@hasura/shared/testing';
import { useRemoteSchemaDeletePrompt } from './useRemoteSchemaDeletePrompt';

// UI integration tests for the remote-schema delete dialog. These drive the
// real dialog (typed-name confirmation + cascade checkbox) and assert what is
// actually sent to `POST /v1/metadata`, so they prove UI behaviour that the
// hook-level payload tests cannot: default-unchecked cascade, opt-in forwarding,
// no API call on cancel/mismatch, and that re-opening resets the checkbox.
const REMOTE_SCHEMA = 'my_remote_schema';

let capturedBodies: any[] = [];

const captureHandler = http.post('*/v1/metadata', async ({ request }) => {
  const body = (await request.clone().json()) as any;
  if (body?.type !== 'export_metadata') {
    capturedBodies.push(body);
  }
  return undefined;
});

const server = setupServer(captureHandler, ...handlers());

const Wrapper = ({ children }: { children?: React.ReactNode }) => (
  <QueryClientProvider
    client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}
  >
    <MemoryRouter>
      <AppContext.Provider
        value={{
          ...defaultAppState,
          envVars: (window as any).__env ?? {},
          endpoints: getEndpoints(
            (window as any).__env ?? {},
            'http://localhost',
          ),
        }}
      >
        <AppTheme>
          <AlertProvider>{children}</AlertProvider>
        </AppTheme>
      </AppContext.Provider>
    </MemoryRouter>
  </QueryClientProvider>
);

const Harness = () => {
  const { promptDelete } = useRemoteSchemaDeletePrompt();
  return <button onClick={() => promptDelete(REMOTE_SCHEMA)}>open</button>;
};

// Radix overlays set `pointer-events: none` on the body; disable the check.
const setupUser = () => userEvent.setup({ pointerEventsCheck: 0 });

const removeRemoteSchemaBodies = () =>
  capturedBodies.filter((b) => b?.type === 'remove_remote_schema');

describe('useRemoteSchemaDeletePrompt (UI integration)', () => {
  beforeAll(() => server.listen());
  beforeEach(() => {
    capturedBodies = [];
    vitest.spyOn(console, 'error').mockImplementation(() => null);
  });
  afterEach(() => {
    vitest.spyOn(console, 'error').mockRestore();
    server.resetHandlers(captureHandler, ...handlers());
  });
  afterAll(() => server.close());

  it('defaults the cascade checkbox to unchecked', async () => {
    const user = setupUser();
    render(
      <Wrapper>
        <Harness />
      </Wrapper>,
    );

    await user.click(screen.getByText('open'));

    const checkbox = await screen.findByRole('checkbox');
    expect(checkbox).not.toBeChecked();
  });

  it('deletes without cascade by default (cascade not sent)', async () => {
    const user = setupUser();
    render(
      <Wrapper>
        <Harness />
      </Wrapper>,
    );

    await user.click(screen.getByText('open'));
    await user.type(await screen.findByRole('textbox'), REMOTE_SCHEMA);
    await user.click(screen.getByTestId('alert-confirm-button'));

    await waitFor(() => expect(removeRemoteSchemaBodies()).toHaveLength(1));
    expect(removeRemoteSchemaBodies()[0].args).toEqual({ name: REMOTE_SCHEMA });
    expect(removeRemoteSchemaBodies()[0].args).not.toHaveProperty('cascade');
  });

  it('forwards cascade: true when the checkbox is opted in', async () => {
    const user = setupUser();
    render(
      <Wrapper>
        <Harness />
      </Wrapper>,
    );

    await user.click(screen.getByText('open'));
    await user.click(await screen.findByRole('checkbox'));
    await user.type(await screen.findByRole('textbox'), REMOTE_SCHEMA);
    await user.click(screen.getByTestId('alert-confirm-button'));

    await waitFor(() => expect(removeRemoteSchemaBodies()).toHaveLength(1));
    expect(removeRemoteSchemaBodies()[0].args).toEqual({
      name: REMOTE_SCHEMA,
      cascade: true,
    });
  });

  it('does not call the API when cancelled', async () => {
    const user = setupUser();
    render(
      <Wrapper>
        <Harness />
      </Wrapper>,
    );

    await user.click(screen.getByText('open'));
    await user.click(await screen.findByRole('checkbox'));
    await user.click(await screen.findByText('Cancel'));

    await new Promise((r) => setTimeout(r, 50));
    expect(removeRemoteSchemaBodies()).toHaveLength(0);
  });

  it('does not call the API when the typed name does not match', async () => {
    const user = setupUser();
    render(
      <Wrapper>
        <Harness />
      </Wrapper>,
    );

    await user.click(screen.getByText('open'));
    await user.type(await screen.findByRole('textbox'), 'WRONG');
    await user.click(screen.getByTestId('alert-confirm-button'));

    expect(await screen.findByText(/does not match/i)).toBeInTheDocument();
    expect(removeRemoteSchemaBodies()).toHaveLength(0);
  });

  it('resets the cascade checkbox to unchecked when re-opened', async () => {
    const user = setupUser();
    render(
      <Wrapper>
        <Harness />
      </Wrapper>,
    );

    // open, tick cascade, then cancel
    await user.click(screen.getByText('open'));
    const firstCheckbox = await screen.findByRole('checkbox');
    await user.click(firstCheckbox);
    expect(firstCheckbox).toBeChecked();
    await user.click(await screen.findByText('Cancel'));

    await waitFor(() =>
      expect(screen.queryByRole('checkbox')).not.toBeInTheDocument(),
    );

    // re-open: the checkbox must default back to unchecked
    await user.click(screen.getByText('open'));
    const secondCheckbox = await screen.findByRole('checkbox');
    expect(secondCheckbox).not.toBeChecked();
  });
});
