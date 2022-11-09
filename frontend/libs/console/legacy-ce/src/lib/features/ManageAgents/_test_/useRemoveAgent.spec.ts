import { renderHook } from '@testing-library/react-hooks';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { wrapper } from '../../../hooks/__tests__/common/decorator';
import { useRemoveAgent } from '../hooks';

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    if ((req.body as Record<string, any>).args.name === 'wrong_payload')
      return res(ctx.status(400), ctx.json({ message: 'Bad request' }));
    return res(ctx.status(200), ctx.json({ message: 'success' }));
  })
);

describe('useRemoveAgent tests: ', () => {
  beforeAll(() => {
    server.listen();
    jest.spyOn(console, 'error').mockImplementation(() => null);
  });
  afterAll(() => {
    server.close();
    jest.spyOn(console, 'error').mockRestore();
  });

  it('calls the custom success callback after adding a DC agent', async () => {
    const { result, waitFor } = renderHook(() => useRemoveAgent(), { wrapper });

    const { removeAgent } = result.current;

    const mockCallback = jest.fn(() => {
      console.log('success');
    });

    removeAgent({
      name: 'test_dc_agent',
      onSuccess: () => {
        mockCallback();
      },
    });

    await waitFor(() => result.current.isSuccess);

    await waitFor(() => {
      expect(mockCallback).toHaveBeenCalledTimes(1);
    });
  });

  it('calls the custom error callback after failing to add a DC agent', async () => {
    const { result, waitFor } = renderHook(() => useRemoveAgent(), { wrapper });

    const { removeAgent } = result.current;

    const mockCallback = jest.fn(() => {
      console.log('error');
    });

    removeAgent({
      name: 'wrong_payload',
      onError: () => {
        mockCallback();
      },
    });

    await waitFor(() => result.current.isError);

    await waitFor(() => {
      expect(mockCallback).toHaveBeenCalledTimes(1);
    });
  });
});
