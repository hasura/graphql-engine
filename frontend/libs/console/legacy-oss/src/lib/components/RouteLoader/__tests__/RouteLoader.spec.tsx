import { render, screen, waitFor } from '@testing-library/react';
import React from 'react';
import { RouteLoader } from '../RouteLoader';

const injectedRouter = {
  push: () => {},
  replace: () => {},
  go: () => {},
  goBack: () => {},
  goForward: () => {},
  setRouteLeaveHook: () => {},
  createPath: () => '',
  createHref: () => '',
  isActive: () => false,
};

describe('RouteLoader', () => {
  it('should display the loader on first render', async () => {
    render(
      <RouteLoader
        loader={<span>loading...</span>}
        location={{}}
        router={injectedRouter}
        onEnter={() => {}}
      >
        <div>Children</div>
      </RouteLoader>
    );

    expect(screen.getByText('loading...')).toBeInTheDocument();
  });
  it('should call the onEnter function', () => {
    const onEnter = jest.fn();
    render(
      <RouteLoader
        loader={<span>loading...</span>}
        location={{}}
        router={injectedRouter}
        onEnter={onEnter}
      >
        <div>Children</div>
      </RouteLoader>
    );
    expect(onEnter).toHaveBeenCalled();
  });
  it('should render children after calling onEnter function', async () => {
    const onEnter = (
      nextState: unknown,
      replaceState: unknown,
      cb?: () => void
    ) => cb?.();
    render(
      <RouteLoader
        loader={<span>loading...</span>}
        location={{}}
        router={injectedRouter}
        onEnter={onEnter}
      >
        <div>Children</div>
      </RouteLoader>
    );
    await waitFor(() => screen.findByText('Children'));
  });
});
