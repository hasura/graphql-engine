import { useEffect, useState } from 'react';
import { LoadingScreen, LoadingScreenTitle } from '@hasura/shared/ui';

// Delay before showing the indicator, so a guard that resolves from cache
// doesn't flash the loader on every navigation.
const SHOW_DELAY_MS = 200;

/**
 * Placeholder rendered in the page area (below the header) while a route
 * guard is still checking the user's privileges.
 */
const RouteGuardLoading = () => {
  const [visible, setVisible] = useState(false);

  useEffect(() => {
    const timer = setTimeout(() => setVisible(true), SHOW_DELAY_MS);
    return () => clearTimeout(timer);
  }, []);

  if (!visible) return null;

  return (
    <LoadingScreen fullScreen={false}>
      <LoadingScreenTitle title="Checking access..." />
    </LoadingScreen>
  );
};

export default RouteGuardLoading;
