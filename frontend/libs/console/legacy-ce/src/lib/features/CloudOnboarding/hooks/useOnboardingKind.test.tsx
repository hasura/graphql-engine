import { renderHook, act } from '@testing-library/react';
import { useOnboardingKind } from './useOnboardingKind';
import { OnboardingResponseData } from '../types';
import { OneClickDeploymentState } from '../OneClickDeployment';

const baseData = (overrides: Record<string, unknown>): OnboardingResponseData =>
  ({
    data: {
      users: [{ created_at: '2023-01-01T00:00:00Z' }],
      user_onboarding: [{ is_onboarded: false }],
      one_click_deployment: [],
      one_click_deployment_sample_apps: [],
      ...overrides,
    },
  }) as unknown as OnboardingResponseData;

describe('useOnboardingKind', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('returns kind "none" while loading', () => {
    // onboardingData must be a stable reference (as react-query provides it)
    const data = baseData({});
    const { result } = renderHook(() => useOnboardingKind(data, null, true));
    expect(result.current.kind).toEqual('none');
  });

  it('derives "use-case-onboarding" for a fresh, not-onboarded user', () => {
    const data = baseData({});
    const { result } = renderHook(() => useOnboardingKind(data, null, false));
    expect(result.current.kind).toEqual('use-case-onboarding');
  });

  it('derives "none" for an already onboarded user', () => {
    const data = baseData({ user_onboarding: [{ is_onboarded: true }] });
    const { result } = renderHook(() => useOnboardingKind(data, null, false));
    expect(result.current.kind).toEqual('none');
  });

  it('derives "one-click-deployment" when a pending deployment exists', () => {
    const data = baseData({
      one_click_deployment: [
        {
          id: 'dep-1',
          state: OneClickDeploymentState.Initialized,
          git_repository_url: 'https://example.com/repo',
          git_repository_branch: 'main',
          hasura_directory: 'hasura',
        },
      ],
    });
    const { result } = renderHook(() => useOnboardingKind(data, null, false));
    expect(result.current.kind).toEqual('one-click-deployment');
  });

  it('recomputes when onboardingData changes from loading to ready', () => {
    const { result, rerender } = renderHook(
      ({ data, loading }: { data: OnboardingResponseData; loading: boolean }) =>
        useOnboardingKind(data, null, loading),
      { initialProps: { data: baseData({}), loading: true } },
    );
    expect(result.current.kind).toEqual('none');

    rerender({ data: baseData({}), loading: false });
    expect(result.current.kind).toEqual('use-case-onboarding');
  });

  it('dismissOnboarding forces kind to none and sticks across re-renders', () => {
    const data = baseData({});
    const { result, rerender } = renderHook(
      ({ d }: { d: OnboardingResponseData }) =>
        useOnboardingKind(d, null, false),
      { initialProps: { d: data } },
    );
    expect(result.current.kind).toEqual('use-case-onboarding');

    act(() => {
      result.current.dismissOnboarding();
    });
    expect(result.current.kind).toEqual('none');

    // re-render with the same inputs must not re-show onboarding
    rerender({ d: data });
    expect(result.current.kind).toEqual('none');
  });

  it('does not loop (bounded renders with unchanged inputs)', () => {
    let hookRuns = 0;
    const data = baseData({});
    const { rerender } = renderHook(() => {
      hookRuns += 1;
      return useOnboardingKind(data, null, false);
    });
    const baseline = hookRuns;
    rerender();
    rerender();
    expect(hookRuns).toBe(baseline + 2);
  });
});
