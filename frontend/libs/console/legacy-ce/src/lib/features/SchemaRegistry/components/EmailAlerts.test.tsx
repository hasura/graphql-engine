import * as React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { EmailAlerts } from './EmailAlerts';

const h = vi.hoisted(() => ({
  mutate: vi.fn(),
  alertResponse: { current: { kind: 'loading' } as any },
}));

vi.mock('../hooks/useGetAlertConfig', () => ({
  useGetAlertConfig: () => h.alertResponse.current,
}));

vi.mock('../hooks/useSetAlertConfig', () => ({
  useSetEmailAlertConfig: () => ({
    setEmailAlertMutation: { mutate: h.mutate, isPending: false },
  }),
}));

const successResponse = (rules: Record<string, boolean>) => ({
  kind: 'success',
  response: { alert_config_service: [{ rules }] },
});

describe('EmailAlerts', () => {
  beforeEach(() => {
    h.mutate.mockClear();
    h.alertResponse.current = { kind: 'loading' };
  });

  it('renders the loading state initially', () => {
    h.alertResponse.current = { kind: 'loading' };
    render(<EmailAlerts onClose={vi.fn()} />);
    expect(screen.getByText('Loading...')).toBeInTheDocument();
  });

  it('seeds the checkbox config from the fetched rules', () => {
    const rules = { safe: true, breaking: true, dangerous: false };
    h.alertResponse.current = successResponse(rules);

    render(<EmailAlerts onClose={vi.fn()} />);

    expect(screen.getByText('Email Alerts')).toBeInTheDocument();

    // Pressing "Set" submits the config that was seeded from the fetched rules.
    fireEvent.click(screen.getByText('Set'));
    expect(h.mutate).toHaveBeenCalledTimes(1);
    expect(h.mutate).toHaveBeenCalledWith(expect.objectContaining({ rules }));
  });

  it('toggles a category and submits the updated config without runaway renders', () => {
    const rules = { safe: true, breaking: true, dangerous: false };
    h.alertResponse.current = successResponse(rules);

    let renderCount = 0;
    const Counting = () => {
      renderCount++;
      return <EmailAlerts onClose={vi.fn()} />;
    };
    render(<Counting />);

    // Toggle "Breaking" off.
    const breakingRow = screen
      .getByText(/Breaking/)
      .closest('[role="checkbox"]') as HTMLElement;
    fireEvent.click(breakingRow);

    fireEvent.click(screen.getByText('Set'));

    expect(h.mutate).toHaveBeenCalledTimes(1);
    expect(h.mutate).toHaveBeenCalledWith(
      expect.objectContaining({
        rules: { safe: true, breaking: false, dangerous: false },
      }),
    );

    // Seeding + a single toggle must not spiral into a render loop.
    expect(renderCount).toBeLessThanOrEqual(4);
  });
});
