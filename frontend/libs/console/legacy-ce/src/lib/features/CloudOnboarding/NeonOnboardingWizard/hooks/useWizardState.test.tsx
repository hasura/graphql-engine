import { renderHook, act } from '@testing-library/react';
import { useWizardState } from './useWizardState';
import { OnboardingResponseData } from '../../types';

// Build onboarding data that `getWizardState` maps to 'landing-page'
// (account created after the onboarding launch date, not yet onboarded).
const makeOnboardingData = (isOnboarded: boolean): OnboardingResponseData =>
  ({
    data: {
      users: [{ created_at: '2023-01-01T00:00:00Z' }],
      user_onboarding: [{ is_onboarded: isOnboarded }],
      one_click_deployment: [],
    },
  }) as unknown as OnboardingResponseData;

describe('useWizardState', () => {
  it('derives the initial state from onboardingData on mount', () => {
    const { result } = renderHook(() =>
      useWizardState(makeOnboardingData(false)),
    );
    expect(result.current.state).toEqual('landing-page');
  });

  it('is hidden when onboardingData is undefined', () => {
    const { result } = renderHook(() => useWizardState(undefined));
    expect(result.current.state).toEqual('hidden');
  });

  it('re-syncs state when onboardingData changes', () => {
    const { result, rerender } = renderHook(
      ({ data }: { data: OnboardingResponseData | undefined }) =>
        useWizardState(data),
      {
        initialProps: { data: undefined as OnboardingResponseData | undefined },
      },
    );
    expect(result.current.state).toEqual('hidden');

    rerender({ data: makeOnboardingData(false) });
    expect(result.current.state).toEqual('landing-page');
  });

  it('preserves manual setState across re-renders with the same onboardingData', () => {
    const data = makeOnboardingData(false);
    const { result, rerender } = renderHook(
      ({ d }: { d: OnboardingResponseData | undefined }) => useWizardState(d),
      { initialProps: { d: data } },
    );
    expect(result.current.state).toEqual('landing-page');

    // consumer manually transitions the wizard
    act(() => {
      result.current.setState('template-summary');
    });
    expect(result.current.state).toEqual('template-summary');

    // a re-render with the SAME onboardingData must not clobber the manual state
    rerender({ d: data });
    expect(result.current.state).toEqual('template-summary');
  });

  it('does not re-render on every render (stable when inputs unchanged)', () => {
    let hookRuns = 0;
    const data = makeOnboardingData(false);
    const { rerender } = renderHook(() => {
      hookRuns += 1;
      return useWizardState(data);
    });
    const baseline = hookRuns;
    rerender();
    rerender();
    // one additional render per rerender call, no runaway state-update loop
    expect(hookRuns).toBe(baseline + 2);
  });
});
