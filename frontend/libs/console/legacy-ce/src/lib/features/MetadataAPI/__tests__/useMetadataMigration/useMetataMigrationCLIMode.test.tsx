/* eslint-disable global-require */
import { renderHook } from '@testing-library/react-hooks';
import { screen, waitFor as testLibWaitFor } from '@testing-library/react';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import React from 'react';
import { Button } from '../../../../new-components/Button';
import { useMetadataMigration } from '../../hooks/useMetadataMigration';
import { useMetadataVersion } from '../../hooks/useMetadataVersion';
import {
  renderWithClient,
  wrapper,
} from '../../../../hooks/__tests__/common/decorator';

const metadata = {
  resource_version: 1,
  sources: [],
};

function setCLIEnvVars() {
  /* eslint no-underscore-dangle: 0 */

  (window as any).__env = {
    ...(window as any).__env,
    consoleMode: 'cli',
    apiHost: 'http://localhost',
    apiPort: '9693',
  };
}

const server = setupServer(
  rest.post('http://localhost:9693/apis/metadata', (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({ message: 'mock success response from cli server' })
    );
  }),
  rest.get('http://localhost:9693/apis/migrate/settings', (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({ message: 'mock success response from cli server' })
    );
  }),
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    if ((req.body as Record<string, any>).type === 'export_metadata') {
      return res(ctx.status(200), ctx.json(metadata));
    }

    if (
      (req.body as Record<string, any>).type === 'pg_create_remote_relationship'
    ) {
      metadata.resource_version += 1;
      return res(
        ctx.status(200),
        ctx.json({ message: 'mock success response from cli server' })
      );
    }
    return undefined;
  })
);

describe('in CLI mode', () => {
  beforeEach(() => {
    setCLIEnvVars();
    jest.spyOn(console, 'error').mockImplementation(() => null);
  });

  afterEach(() => {
    jest.spyOn(console, 'error').mockRestore();
  });

  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should increment metadata version by 1 after successful mutatation', async () => {
    const onSuccessMock = jest.fn();
    const onMutationSuccessMock = jest.fn();

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

    renderWithClient(<Page />);

    await testLibWaitFor(() => {
      expect(onSuccessMock).toHaveBeenCalledTimes(1);
    });

    await testLibWaitFor(() => {
      expect(screen.getByRole('heading')).toMatchInlineSnapshot(`
        <h1>
          1
        </h1>
      `);
    });

    screen.getByRole('button', { name: /mutate/i }).click();

    await testLibWaitFor(() => {
      expect(onMutationSuccessMock).toHaveBeenCalledTimes(1);
    });

    await testLibWaitFor(() => {
      expect(onSuccessMock).toHaveBeenCalledTimes(5);
    });

    await testLibWaitFor(() => {
      expect(screen.getByRole('heading')).toMatchInlineSnapshot(`
              <h1>
                2
              </h1>
          `);
    });
  });

  it('should call the /apis/migrate when console is running in cli mode', async () => {
    const { result, waitFor } = renderHook(() => useMetadataMigration(), {
      wrapper,
    });

    result.current.mutate({
      query: { type: 'pg_create_remote_relationship', args: {} },
    });
    await waitFor(() => result.current.isSuccess);

    expect(result.current?.data).toMatchInlineSnapshot(`
      {
        "message": "mock success response from cli server",
      }
    `);
  });
});
