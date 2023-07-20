import * as React from 'react';
import { render, screen, cleanup, fireEvent } from '@testing-library/react';
import { renderHook, act } from '@testing-library/react-hooks';
import { QueryClient, QueryClientProvider } from 'react-query';
import type { GraphQLError } from 'graphql';
import { ExchangeTokenResponse, useSlackOAuth } from './useSlackOAuth';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import 'whatwg-fetch';

import { SLACK_CALLBACK_SEARCH } from '../utils';

function TestComponent() {
  const { startSlackOAuth, slackOauthStatus, oauth2State } = useSlackOAuth();
  return (
    <div>
      <p data-testid="status">{slackOauthStatus.status}</p>
      <p data-testid="error">
        {slackOauthStatus.status === 'error'
          ? slackOauthStatus.error.message
          : ''}
      </p>
      <p data-testid="channelName">
        {slackOauthStatus.status === 'authenticated'
          ? slackOauthStatus.channelName
          : ''}
      </p>
      <p data-testid="oauth2-state">{oauth2State}</p>
      <button data-testid="start" onClick={startSlackOAuth}>
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
const server = setupServer();
server.events.on('response:mocked', () => {
  //
  jest.advanceTimersByTime(10);
  jest.runAllTicks();
});

const mockHTTPResponse = (status = 200, returnBody: any) => {
  server.use(
    rest.all('*', (req, res, context) => {
      return res(context.json(returnBody), context.status(status));
    })
  );
};

type Props = {
  children?: React.ReactNode;
};
const queryClient = new QueryClient();
const wrapper = ({ children }: Props) => {
  // const reactQueryClient = useQueryClient();

  return (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
};

// --------------------------------------------------
// TEST UTILS
// --------------------------------------------------
// use fake timers because the hook uses setTimeout and setInterval

function closeFakePopup() {
  act(() => {
    mockPopupImpl.closePopup();
    jest.advanceTimersByTime(4000);
    jest.runAllTicks();
  });
}

// --------------------------------------------------
// TESTS
// --------------------------------------------------
describe('Slack', () => {
  // reset test state after each test
  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
    jest.useRealTimers();
  });
  beforeEach(() => {
    cleanup();
    mockLocalStorage.clear();
    mockPopupImpl.closePopup();

    jest.useFakeTimers();
    jest.clearAllTimers();
    jest.clearAllMocks();
    server.resetHandlers();
  });

  it('Happy path', async () => {
    // Arrange
    const { waitForValueToChange, result } = renderHook(() => useSlackOAuth(), {
      wrapper,
    });

    expect(result.current.slackOauthStatus.status).toEqual('idle');

    // Act
    act(() => {
      result.current.startSlackOAuth();
    });
    // Wait for the loading state to be triggered
    expect(result.current.slackOauthStatus.status).toEqual('authenticating');

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params in local storage and close popup
    // mock success exchange of token
    const response: ExchangeTokenResponse = {
      data: {
        slackExchangeOAuthToken: {
          channel_name: '#test-channel',
          team_name: 'test-team',
        },
      },
    };
    mockHTTPResponse(200, response);

    const oauth2State = result.current.oauth2State;
    mockLocalStorage.setItem(
      SLACK_CALLBACK_SEARCH,
      `code=test_code&state=${oauth2State}`
    );

    closeFakePopup();
    jest.runAllTicks();
    // --------------------------------------------------

    // --------------------------------------------------

    // ALl good until here
    // Assert
    await waitForValueToChange(() => result.current.slackOauthStatus);

    expect(result.current.slackOauthStatus.status).toEqual('authenticated');

    // @ts-expect-error we know better than typescript here
    expect(result.current.slackOauthStatus?.channelName).toEqual(
      response.data?.slackExchangeOAuthToken.channel_name
    );
  });

  it('Renders idle state correctly', () => {
    // Arrange
    const { result } = renderHook(() => useSlackOAuth(), { wrapper });
    // Assert
    expect(result.current.slackOauthStatus.status).toEqual('idle');
  });

  it('throws unexpected error when the popup is closed before the parameters are stored in localstorage', () => {
    // Arrange
    render(<TestComponent />, { wrapper });

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    closeFakePopup();
    // --------------------------------------------------

    // Assert
    expect(screen.getByTestId('error')).toHaveTextContent(
      'Slack integration closed unexpectedly. Please try again.'
    );
  });

  it('throws OAuth error when the OAuth code does not exist in search params in local storage', () => {
    // Arrange
    render(<TestComponent />, { wrapper });

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params in localstorage and close popup
    // code is not set in search params
    // results in authentication error
    mockLocalStorage.setItem(SLACK_CALLBACK_SEARCH, 'state=test_state');
    closeFakePopup();
    // --------------------------------------------------

    // Assert
    expect(screen.getByTestId('error')).toHaveTextContent(
      'Error authenticating with Slack. Please try again.'
    );
  });

  it('Throw forgery error when the OAuth state mismatch', () => {
    // Arrange
    render(<TestComponent />, { wrapper });

    // Act
    fireEvent.click(screen.getByTestId('start'));

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set search params localstorage and close popup
    // results in state mismatch error
    mockLocalStorage.setItem(
      SLACK_CALLBACK_SEARCH,
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
    const { waitForValueToChange, result } = renderHook(() => useSlackOAuth(), {
      wrapper,
    });

    // Act
    act(() => {
      result.current.startSlackOAuth();
    });

    // --------------------------------------------------
    // CONTROLLING TEST MOCKS
    // set the right in local storage along with a code
    const oauth2State = result.current.oauth2State;
    mockLocalStorage.setItem(
      SLACK_CALLBACK_SEARCH,
      `code=test_code&state=${oauth2State}`
    );

    const response: { errors: GraphQLError[] } = {
      // @ts-expect-error The error format seems not to match the declared type returned by
      // controlPlaneDataApiClient, we should fix it
      errors: [{ message: 'Something went wrong while integrating Slack.' }],
    };

    // mock HTTP response for exchanging token to return an exchange error
    mockHTTPResponse(200, response);

    closeFakePopup();
    // --------------------------------------------------

    // Assert

    await waitForValueToChange(() => result.current.slackOauthStatus);
    console.log(result.current.slackOauthStatus);

    expect(result.current.slackOauthStatus.status).toEqual('error');

    // @ts-expect-error we know better than typescript here
    expect(result.current.slackOauthStatus?.error?.message).toEqual(
      'Something went wrong while integrating Slack.'
    );
  });
});
