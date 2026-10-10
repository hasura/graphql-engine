import {
  renderHook,
  screen,
  waitFor as testLibWaitFor,
} from '@testing-library/react';
import { setupServer } from 'msw/node';
import { Button } from '@hasura/shared/ui';
import {
  handlers,
  testRenderWithClient,
  testWrapper,
} from '@hasura/shared/testing';
import { useMetadataMigration } from '../useMetadataMigration';
import { useMetadataVersion } from '../useMetadataVersion';

// In CLI mode the mutation still hits `${baseUrl}/v1/metadata`; the only extra
// call is `usePostMetadataMigration` exporting metadata from the CLI server
// (`GET .../apis/metadata`). The shared `handlers()` mock both, matched by path,
// so `http://localhost:9693/apis/metadata` is covered too.
const server = setupServer(...handlers());

function setCLIEnvVars() {
  /* eslint no-underscore-dangle: 0 */

  (window as any).__env = {
    ...(window as any).__env,
    consoleMode: 'cli',
    apiHost: 'http://localhost',
    apiPort: '9693',
  };
}

describe('in CLI mode', () => {
  beforeEach(() => {
    setCLIEnvVars();
    vitest.spyOn(console, 'error').mockImplementation(() => null);
  });

  afterEach(() => {
    vitest.spyOn(console, 'error').mockRestore();
    server.resetHandlers(...handlers());
  });

  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should increment metadata version by 1 after successful mutatation', async () => {
    const onSuccessMock = vitest.fn();
    const onMutationSuccessMock = vitest.fn();

    function Page() {
      const mutationCallBack = () => {
        onMutationSuccessMock();
      };

      const mutation = useMetadataMigration({ onSuccess: mutationCallBack });
      const query = useMetadataVersion();

      if (query.isSuccess) onSuccessMock();

      return (
        <>
          <Button
            onClick={() => {
              mutation.mutate({
                query: { type: 'pg_create_remote_relationship', args: {} },
              });
            }}
          >
            Mutate
          </Button>
          <h1>{query.isSuccess ? JSON.stringify(query.data) : 'NA'}</h1>
        </>
      );
    }

    testRenderWithClient(<Page />);

    await testLibWaitFor(() => {
      expect(onSuccessMock).toHaveBeenCalledTimes(1);
    });

    await testLibWaitFor(() => {
      expect(screen.getByRole('heading')).toHaveTextContent('1');
    });
    expect(screen.getByRole('heading')).toMatchInlineSnapshot(`
        <h1>
          1
        </h1>
      `);

    screen.getByRole('button', { name: /mutate/i }).click();

    await testLibWaitFor(() => {
      expect(onMutationSuccessMock).toHaveBeenCalledTimes(1);
    });

    await testLibWaitFor(() => {
      expect(screen.getByRole('heading')).toHaveTextContent('2');
    });

    // the exact number of renders where the query is successful is an
    // implementation detail of react-query's internals (and changed between
    // major versions); what matters is that it re-rendered with the new data.
    expect(onSuccessMock.mock.calls.length).toBeGreaterThan(1);

    expect(screen.getByRole('heading')).toMatchInlineSnapshot(`
              <h1>
                2
              </h1>
          `);
  });

  it('should call the metadata endpoint when console is running in cli mode', async () => {
    const { result } = renderHook(() => useMetadataMigration(), {
      wrapper: testWrapper,
    });

    result.current.mutate({
      query: { type: 'pg_create_remote_relationship', args: {} },
    });
    await testLibWaitFor(() => expect(result.current.isSuccess).toBe(true));

    expect(result.current?.data).toMatchInlineSnapshot(`
      {
        "message": "success",
      }
    `);
  });
});
