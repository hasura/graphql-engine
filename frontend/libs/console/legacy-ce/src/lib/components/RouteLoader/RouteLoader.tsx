import { onEnterHook } from '../../utils/router';
import { LocationDescriptorObject } from 'history';
import React from 'react';
import { InjectedRouter, withRouter } from 'react-router';

export const RouteLoader = withRouter(
  ({
    children,
    onEnter,
    location,
    router,
    loader,
  }: {
    children: React.ReactElement;
    onEnter: onEnterHook;
    router: InjectedRouter;
    location: LocationDescriptorObject;
    loader?: React.ReactElement;
  }) => {
    const [loading, setLoading] = React.useState<boolean>(true);
    const nextState = { location };
    const replaceState = router.push;
    const cb = () => setLoading(false);
    React.useEffect(() => {
      onEnter(nextState, replaceState, cb);
    }, []);
    if (loading) return loader ?? null;
    return children;
  }
);
