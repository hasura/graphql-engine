import React from 'react';
import { Breadcrumbs } from '../../../../new-components/Breadcrumbs';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks';
import { NATIVE_QUERY_ROUTE_DETAIL } from '../constants';
import { injectRouteDetails, pathsToBreadcrumbs } from './route-wrapper-utils';

export type RouteWrapperProps = {
  route: keyof typeof NATIVE_QUERY_ROUTE_DETAIL;
  itemSourceName?: string;
  itemName?: string;
  itemTabName?: string;
  subtitle?: string;
};

export const RouteWrapper: React.FC<RouteWrapperProps> = props => {
  const { children, route, subtitle: subtitleOverride } = props;

  const paths = route?.split('/').filter(Boolean);

  const { title, subtitle, docLink } = NATIVE_QUERY_ROUTE_DETAIL[route];

  const push = usePushRoute();

  return (
    <div className="py-md px-md w-full">
      <div className="flex flex-col">
        <Breadcrumbs items={pathsToBreadcrumbs(paths, props, push)} />
        <div className="flex w-full justify-between px-2">
          <div className="mb-sm">
            <div className="text-xl font-bold mt-2">
              {injectRouteDetails(title, props)}
            </div>
            <div className="text-muted">
              {subtitleOverride ?? subtitle}{' '}
              {docLink && <LearnMoreLink href={docLink} />}
            </div>
          </div>
        </div>
      </div>
      <div className="">{children}</div>
    </div>
  );
};
