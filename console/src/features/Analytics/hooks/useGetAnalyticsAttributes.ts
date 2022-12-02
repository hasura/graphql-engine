import type { AnalyticsOptions } from '../core/getAnalyticsAttributes';
import { getAnalyticsAttributes } from '../core/getAnalyticsAttributes';

/**
 * The hook-ish version of getAnalyticsAttributes.
 * The default choice should be to use the <Analytics /> component but this hook can be used in
 * cases where the analytics name is optional.
 */
export function useGetAnalyticsAttributes(
  name?: string,
  options?: AnalyticsOptions
) {
  // Since the custom hook is meant for custom React-based usage, the optional `name` allows React-based
  // consumers not caring about the hook not being callable optionally.
  if (!name) return {};

  return getAnalyticsAttributes(name, options);
}
