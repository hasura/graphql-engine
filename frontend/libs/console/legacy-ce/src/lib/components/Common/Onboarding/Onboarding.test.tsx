import { render, screen } from '@testing-library/react';
import { vi } from 'vitest';
import Onboarding from './index';

const { getLSItemMock } = vi.hoisted(() => ({ getLSItemMock: vi.fn() }));

vi.mock('@hasura/shared/utils', async (importOriginal) => ({
  ...(await importOriginal<Record<string, unknown>>()),
  isCloudConsole: () => false,
  getLSItem: (...args: unknown[]) => getLSItemMock(...args),
  setLSItem: () => undefined,
}));

vi.mock('@hasura/shared/context', async (importOriginal) => ({
  ...(await importOriginal<Record<string, unknown>>()),
  useAppContext: () => ({ envVars: {}, serverVersion: '' }),
}));

vi.mock('../../../telemetry', () => ({
  useCatalogState: () => ({ setOnboardingCompleted: vi.fn() }),
}));

vi.mock('react-youtube', () => ({
  default: () => <div data-testid="youtube" />,
}));

const consoleOpts = { onboardingShown: false } as never;

describe('Onboarding visibility (lazy init from local storage)', () => {
  beforeEach(() => getLSItemMock.mockReset());

  it('shows the popup on initial render when local storage is empty (defaults to visible)', () => {
    getLSItemMock.mockReturnValue(null);

    render(<Onboarding console_opts={consoleOpts} metadata={undefined} />);

    expect(screen.getByText(/get started with Hasura/i)).toBeInTheDocument();
  });

  it('shows the popup when local storage says "true"', () => {
    getLSItemMock.mockReturnValue('true');

    render(<Onboarding console_opts={consoleOpts} metadata={undefined} />);

    expect(screen.getByText(/get started with Hasura/i)).toBeInTheDocument();
  });

  it('hides the popup on initial render when local storage says "false"', () => {
    getLSItemMock.mockReturnValue('false');

    render(<Onboarding console_opts={consoleOpts} metadata={undefined} />);

    // popup hidden, only the "Hi" wave icon is shown
    expect(
      screen.queryByText(/get started with Hasura/i),
    ).not.toBeInTheDocument();
    expect(screen.getByLabelText('Wave')).toBeInTheDocument();
  });
});
