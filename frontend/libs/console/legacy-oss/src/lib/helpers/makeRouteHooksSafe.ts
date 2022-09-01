import { createRoutes } from 'react-router/lib/RouteUtils';
import { PlainRoute, EnterHook } from 'react-router';

// Wrap the hooks so they don't fire if they're called before
// the store is initialised. This only happens when doing the first
// client render of a route that has an onEnter hook

function makeHooksSafe(routes: PlainRoute[] | PlainRoute, store: any): unknown {
  if (Array.isArray(routes)) {
    return routes.map(route => makeHooksSafe(route, store));
  }

  const onEnter = routes.onEnter;

  if (onEnter) {
    // eslint-disable-next-line no-param-reassign
    routes.onEnter = function safeOnEnter(...args: Parameters<EnterHook>) {
      try {
        store.getState();
      } catch (err) {
        if (onEnter.length === 3 && args[2]) {
          args[2]();
        }

        // There's no store yet so ignore the hook
        return;
      }

      // eslint-disable-next-line prefer-spread
      onEnter.apply(null, args);
    };
  }

  if (routes.childRoutes) {
    makeHooksSafe(routes.childRoutes, store);
  }

  if (routes.indexRoute) {
    makeHooksSafe(routes.indexRoute, store);
  }

  return routes;
}

export default function makeRouteHooksSafe(_getRoutes: any) {
  return (store: any) => makeHooksSafe(createRoutes(_getRoutes(store)), store);
}
