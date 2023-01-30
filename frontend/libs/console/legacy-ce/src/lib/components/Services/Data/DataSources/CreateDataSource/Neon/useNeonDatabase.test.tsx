import * as React from 'react';
import { GraphQLError } from 'graphql';
import {
  act,
  render,
  screen,
  cleanup,
  waitFor,
  fireEvent,
} from '@testing-library/react';
import { useNeonDatabase } from './useNeonDatabase';

const TestComponent = () => {
  const { create, state } = useNeonDatabase();

  return (
    <div>
      <p data-testid="status">{state.status}</p>
      {state.status === 'error' && <p data-testid="error">{state.error}</p>}
      {state.status === 'success' && (
        <p data-testid="payload">{JSON.stringify(state.payload)}</p>
      )}
      <button data-testid="start" onClick={create}>
        start
      </button>
    </div>
  );
};

// --------------------------------------------------
// NETWORK MOCK
// --------------------------------------------------
const mockHTTPResponse = (status = 200, returnBody: any) => {
  global.fetch = jest.fn().mockImplementationOnce(() => {
    return new Promise(resolve => {
      resolve({
        ok: true,
        status,
        json: () => {
          return returnBody || {};
        },
        headers: {
          get: (key: string) =>
            key === 'Content-Type' ? 'application/json' : '',
        },
      });
    });
  });
};

jest.useFakeTimers();
beforeEach(() => {
  jest.clearAllMocks();
  jest.clearAllTimers();
  cleanup();
});

describe('useNeonDatabase', () => {
  it('Renders idle state correctly', () => {
    render(<TestComponent />);
    expect(screen.getByTestId('status')).toHaveTextContent('idle');
  });

  it('Creates DB correctly', async () => {
    const creationResponse = {
      data: {
        neonCreateDatabase: {
          isAuthenticated: true,
          databaseUrl: 'db-url',
          email: 'email@test.com',
          envVar: 'TEST',
        },
      },
    };
    mockHTTPResponse(200, creationResponse);
    await render(<TestComponent />);
    act(() => {
      fireEvent.click(screen.getByTestId('start'));
    });
    await waitFor(() => {
      expect(screen.getByTestId('status')).toHaveTextContent('loading');
    });
    jest.setTimeout(1000);
    await waitFor(() => {
      expect(screen.getByTestId('status')).toHaveTextContent('success');
    });
    expect(screen.getByTestId('payload')).toHaveTextContent(
      JSON.stringify(creationResponse.data.neonCreateDatabase)
    );
  });

  it('Propogates error state correctly', async () => {
    const creationResponse = { errors: [new GraphQLError('api error')] };
    mockHTTPResponse(200, creationResponse);
    await render(<TestComponent />);
    act(() => {
      fireEvent.click(screen.getByTestId('start'));
    });
    await waitFor(() => {
      expect(screen.getByTestId('status')).toHaveTextContent('loading');
    });
    await waitFor(() => {
      expect(screen.getByTestId('error'));
    });
    expect(screen.getByTestId('error')).toHaveTextContent('api error');
  });
});
