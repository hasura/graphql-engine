import * as React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { SlackAlerts } from './SlackAlerts';

const renderWithTheme = (ui: React.ReactElement) => render(<Theme>{ui}</Theme>);

const h = vi.hoisted(() => ({
  startSlackOAuth: vi.fn(),
  deleteMutate: vi.fn(),
  oauth: { current: { status: 'idle' } as any },
  slackState: {
    current: { kind: 'success', response: { slack_config: [] } } as any,
  },
}));

vi.mock('../hooks/useSlackIntegration', () => ({
  useSlackIntegration: () => ({
    slackOauthStatus: h.oauth.current,
    startSlackOAuth: h.startSlackOAuth,
    deleteSlackApp: { mutate: h.deleteMutate },
  }),
}));

vi.mock('../hooks/useGetSlackState', () => ({
  useGetSlackState: () => h.slackState.current,
}));

describe('SlackAlerts', () => {
  beforeEach(() => {
    h.startSlackOAuth.mockClear();
    h.deleteMutate.mockClear();
    h.oauth.current = { status: 'idle' };
    h.slackState.current = { kind: 'success', response: { slack_config: [] } };
  });

  it('renders the idle "Add to Slack" state initially', () => {
    renderWithTheme(<SlackAlerts onClose={vi.fn()} />);
    expect(screen.getByText('Add to Slack')).toBeInTheDocument();
  });

  it('mirrors an error status coming from the integration hook', () => {
    h.oauth.current = { status: 'error', error: new Error('boom') };
    renderWithTheme(<SlackAlerts onClose={vi.fn()} />);
    expect(screen.getByText('boom')).toBeInTheDocument();
  });

  it('forces idle when authenticated but no integration exists', () => {
    // doesSlackIntegrationExist is false (empty slack_config), oauth authenticated
    h.oauth.current = {
      status: 'authenticated',
      channelName: '#c',
      teamName: 't',
    };
    renderWithTheme(<SlackAlerts onClose={vi.fn()} />);
    // sync forces idle -> "Add to Slack" shown rather than the authenticated tile
    expect(screen.getByText('Add to Slack')).toBeInTheDocument();
  });

  it('dismisses an error back to idle via the dismiss action', () => {
    h.oauth.current = { status: 'error', error: new Error('boom') };
    renderWithTheme(<SlackAlerts onClose={vi.fn()} />);
    expect(screen.getByText('boom')).toBeInTheDocument();

    const dismiss = document.querySelector('svg.cursor-pointer') as SVGElement;
    fireEvent.click(dismiss);

    // After dismiss, local state becomes idle; the OAuth status is unchanged so
    // it is not re-synced -> the idle "Add to Slack" state is shown.
    expect(screen.getByText('Add to Slack')).toBeInTheDocument();
  });

  it('does not re-render in a loop for stable inputs', () => {
    let renderCount = 0;
    const Counting = () => {
      renderCount++;
      return <SlackAlerts onClose={vi.fn()} />;
    };
    renderWithTheme(<Counting />);
    expect(screen.getByText('Add to Slack')).toBeInTheDocument();
    expect(renderCount).toBeLessThanOrEqual(3);
  });
});
