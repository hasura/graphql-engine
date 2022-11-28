import * as React from 'react';
import {
  act,
  render,
  screen,
  cleanup,
  waitFor,
  fireEvent,
} from '@testing-library/react';
import type { GraphQLError } from 'graphql';
import { ExchangeTokenResponse, useNeonOAuth } from './useNeonOAuth';

import { NEON_CALLBACK_SEARCH } from './utils';

function TestComponent() {
  const { startNeonOAuth, neonOauthStatus, oauth2State } = useNeonOAuth();
  return (
    <div>
      <p data-testid="status">{neonOauthStatus.status}</p>
      <p data-testid="error">
        {neonOauthStatus.status === 'error'
          ? neonOauthStatus.error.message
          : ''}
      </p>
      <p data-testid="email">
        {neonOauthStatus.status === 'authenticated'
          ? neonOauthStatus.email
          : ''}
      </p>
      <p data-testid="oauth2-state">{oauth2State}</p>
      <button data-testid="start" onClick={startNeonOAuth}>
        start
      </button>
    </div>
  );
}

// --------------------------------------------------
// LOCALSTORAGE MOCK
// --------------------------------------------------
const getMockLocalStorage = () => {
  let store: Record<string, string> = {};
  return {
    getItem(key: string): string | undefined {
      return store[key];
    },
    setItem(key: string, value: string) {
      store[key] = value.toString();
    },
    clear() {
      store = {};
    },
    removeItem(key: string) {
      delete store[key];
    },
  };
};
const mockLocalStorage = getMockLocalStorage();
Object.defineProperty(window, 'localStorage', { value: mockLocalStorage });

// --------------------------------------------------
// POPUP MOCK
// --------------------------------------------------
const getMockPopupImpl = () => {
  const popup = {
    closed: false,
  };
  const openPopup = () => {
    popup.closed = false;
    return popup;
  };
  const closePopup = () => {
    popup.closed = true;
    return popup;
  };
  return {
    openPopup,
    closePopup,
  };
};
const mockPopupImpl = getMockPopupImpl();
Object.defineProperty(window, 'open', { value: mockPopupImpl.openPopup });

// --------------------------------------------------
// NETWORK MOCK
// --------------------------------------------------
const mockHTTPResponse = (status = 200, returnBody: any) => {
  global.fetch = jest.fn().mockImplementationOnce(() => {
    return new Promise(resolve => {
      setTimeout(() => {
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
      }, 1000);
    });
  });
};

// --------------------------------------------------
// TEST UTILS
// --------------------------------------------------
// use fake timers because the hook uses setTimeout and setInterval
jest.useFakeTimers();

function closeFakePopup() {
  act(() => {
    mockPopupImpl.closePopup();
    jest.advanceTimersByTime(2000);
  });
}

// --------------------------------------------------
// TESTS
// --------------------------------------------------
describe('Neon', () => {
  // reset test state after each test
  beforeEach(() => {
    cleanup();
    mockLocalStorage.clear();
    mockPopupImpl.closePopup();
    jest.clearAllTimers();
    jest.clearAllMocks();
  });

  it('Happy path', async () => {
    // Arrange
    render(<TestComponent />);

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // Wait for the loading state to be triggered
    await waitFor(() => {
      expect(screen.getByTestId('status')).toHaveTextContent('authenticating');
    });

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params in local storage and close popup
    // mock success exchange of token
    const oauth2State = screen.getByTestId('oauth2-state').textContent;
    mockLocalStorage.setItem(
      NEON_CALLBACK_SEARCH,
      `code=test_code&state=${oauth2State}`
    );

    const response: ExchangeTokenResponse = {
      data: {
        neonExchangeOAuthToken: {
          accessToken: 'test_token',
          email: 'test@email.com',
        },
      },
    };
    mockHTTPResponse(200, response);

    closeFakePopup();
    // --------------------------------------------------

    // Assert
    await waitFor(() => {
      expect(screen.getByTestId('status')).toHaveTextContent('authenticated');
    });
    expect(screen.getByTestId('email')).toHaveTextContent(
      response.data.neonExchangeOAuthToken.email
    );
  });

  it('Renders idle state correctly', () => {
    // Arrange
    render(<TestComponent />);
    // Assert
    expect(screen.getByTestId('status')).toHaveTextContent('idle');
  });

  it('throws unexpected error when the popup is closed before the parameters are stored in localstorage', () => {
    // Arrange
    render(<TestComponent />);

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    closeFakePopup();
    // --------------------------------------------------

    // Assert
    expect(screen.getByTestId('error')).toHaveTextContent(
      'Neon login closed unexpectedly. Please try again.'
    );
  });

  it('throws OAuth error when the OAuth code does not exist in search params in local storage', () => {
    // Arrange
    render(<TestComponent />);

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params in localstorage and close popup
    // code is not set in search params
    // results in authentication error
    mockLocalStorage.setItem(NEON_CALLBACK_SEARCH, 'state=test_state');
    closeFakePopup();
    // --------------------------------------------------

    // Assert
    expect(screen.getByTestId('error')).toHaveTextContent(
      'Error authenticating with Neon. Please try again.'
    );
  });

  it('Throw forgery error when the OAuth state mismatch', () => {
    // Arrange
    render(<TestComponent />);

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params localstorage and close popup
    // results in state mismatch error
    mockLocalStorage.setItem(
      NEON_CALLBACK_SEARCH,
      'code=test_code&state=test_state'
    );
    closeFakePopup();
    // --------------------------------------------------

    // Assert
    expect(screen.getByTestId('error')).toHaveTextContent(
      'Invalid OAuth session state. Please try again.'
    );
  });

  it('Renders oauth error when there is an error exchanging the token', async () => {
    // Arrange
    render(<TestComponent />);

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set the right in local storage along with a code
    const oauth2State = screen.getByTestId('oauth2-state').textContent;
    mockLocalStorage.setItem(
      NEON_CALLBACK_SEARCH,
      `code=test_code&state=${oauth2State}`
    );

    const response: { errors: GraphQLError[] } = {
      // @ts-expect-error The error format seems not to match the declared type returned by
      // controlPlaneDataApiClient, we should fix it
      errors: [{ message: 'oauth api error' }],
    };

    // mock HTTP response for exchanging token to return an exchange error
    mockHTTPResponse(200, response);

    closeFakePopup();
    // --------------------------------------------------

    // Assert
    await waitFor(() => {
      expect(screen.getByTestId('error')).toHaveTextContent('oauth api error');
    });
  });
});
