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
import type { ExchangeTokenResponse } from './useNeonOAuth';

import { NEON_CALLBACK_SEARCH } from './utils';

import { Neon } from '.';

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

// --------------------------------------------------
// TEST UTILS
// --------------------------------------------------
// use fake timers because the hook uses setTimeout and setInterval
jest.useFakeTimers();

function closeFakePopup() {
  act(() => {
    mockPopupImpl.closePopup();
    jest.advanceTimersByTime(1000);
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
    render(<Neon oauthString="FAKE_OAUTH_TOKEN" />);

    // Act
    fireEvent.click(screen.getByText('Connect to Neon'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params in local storage and close popup
    // mock success exchange of token
    mockLocalStorage.setItem(
      NEON_CALLBACK_SEARCH,
      `code=test_code&state=FAKE_OAUTH_TOKEN`
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
      expect(
        screen.getByText('Authenticated. Hello test@email.com!')
      ).toBeInTheDocument();
    });
  });

  it('When rendered, then does not throw errors', () => {
    // Arrange
    render(<Neon />);
    // Assert
    expect(screen.getByText('Connect to Neon')).toBeInTheDocument();
  });

  it('When clicked on Connect to Neon, then starts authenticating', () => {
    // Arrange
    render(<Neon />);

    // Act
    fireEvent.click(screen.getByText('Connect to Neon'));

    // Assert
    expect(
      screen.getByText('Authenticating with Neon DB...')
    ).toBeInTheDocument();
  });

  it('When the popup is closed before the parameters are stored in localstorage, then renders an unexpected error', () => {
    // Arrange
    render(<Neon />);

    // Act
    fireEvent.click(screen.getByText('Connect to Neon'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    closeFakePopup();
    // --------------------------------------------------

    // Assert
    expect(
      screen.getByText(
        'Error authenticating: Unexpected error. Please try again.'
      )
    ).toBeInTheDocument();
  });

  it('When the OAuth code does not exist, then renders an oauth error', () => {
    // Arrange
    render(<Neon />);

    // Act
    fireEvent.click(screen.getByText('Connect to Neon'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params in localstorage and close popup
    // code is not set in search params
    // results in authentication error
    mockLocalStorage.setItem(NEON_CALLBACK_SEARCH, 'state=test_state');
    closeFakePopup();
    // --------------------------------------------------

    // Assert
    expect(
      screen.getByText(
        'Error authenticating: Error authenticating with Neon. Please try again.'
      )
    ).toBeInTheDocument();
  });

  it('When the OAuth code mismatch, then renders an oauth error', () => {
    // Arrange
    render(<Neon />);

    // Act
    fireEvent.click(screen.getByText('Connect to Neon'));

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
    expect(
      screen.getByText(
        'Error authenticating: Invalid OAuth session state. Please try again.'
      )
    ).toBeInTheDocument();
  });

  it('When there is an error exchanging the token, then renders an oauth error', async () => {
    // Arrange
    render(<Neon />);

    // Act
    fireEvent.click(screen.getByText('Connect to Neon'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set the right in local storage along with a code
    mockLocalStorage.setItem(
      NEON_CALLBACK_SEARCH,
      `code=test_code&state=INVALID`
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
    expect(
      screen.getByText(
        'Error authenticating: Invalid OAuth session state. Please try again.'
      )
    ).toBeInTheDocument();
  });
});
