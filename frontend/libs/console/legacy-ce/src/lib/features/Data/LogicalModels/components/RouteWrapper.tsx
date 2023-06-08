import startCase from 'lodash/startCase';
import React, { ReactNode } from 'react';
import Skeleton from 'react-loading-skeleton';
import { useServerConfig } from '../../../../hooks';
import { Breadcrumbs } from '../../../../new-components/Breadcrumbs';
import { LimitedFeatureWrapper } from '../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks';
import { NATIVE_QUERY_ROUTES } from '../constants';

function NativeQueriesFeatureFlag({ children }: { children: ReactNode }) {
  const { data: serverConfig, isLoading: isLoadingServerConfig } =
    useServerConfig();
  if (isLoadingServerConfig) {
    return <Skeleton count={2} />;
  }
  const areNativeQueriesEnabled = serverConfig?.feature_flags?.find(
    feature => feature.name === 'native-query-interface' && feature.enabled
  );
  if (!areNativeQueriesEnabled) {
    return (
      <div className="bg-white border-2 shadow-sm p-5 rounded space-y-4">
        <div className="text-xl text-slate-900">
          Looking to try Native Queries?
        </div>
        <div className="text-base text-muted max-w-2xl">
          Native Queries are not enabled. To enable them, start the Hasura
          server with the environment variable
          <code>HASURA_FF_NATIVE_QUERY_INTERFACE: 'True'</code>
        </div>
      </div>
    );
  }

  return <>{children}</>;
}

export const RouteWrapper: React.FC<{
  route: keyof typeof NATIVE_QUERY_ROUTES;
  itemSourceName?: string;
  itemName?: string;
}> = ({ children, route, itemSourceName, itemName }) => {
  const paths =
    route
      ?.split('/')
      .filter(Boolean)
      .filter(p => p !== '{{source}}') ?? [];

  const { title, subtitle } = NATIVE_QUERY_ROUTES[route];

  const push = usePushRoute();

  return (
    <div className="py-md px-md w-full">
      <LimitedFeatureWrapper
        title="Looking to add Native Queries?"
        id="native-queries"
        description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
      >
        <NativeQueriesFeatureFlag>
          <div className="flex flex-col">
            <Breadcrumbs
              items={paths.map((path: string, index) => {
                return {
                  title: startCase(
                    path
                      // we don't need to display source
                      .replace('{{source}}', itemSourceName ?? '')
                      .replace('{{name}}', itemName ?? '')
                  ),
                  onClick:
                    index === paths.length - 1
                      ? undefined
                      : () => {
                          push(`/${paths.slice(0, index + 1).join('/')}`);
                        },
                };
              })}
            />
            <div className="flex w-full justify-between px-2">
              <div className="mb-sm">
                <div className="text-xl font-bold mt-2">
                  {title.replace('{{name}}', itemName ?? '')}
                </div>
                <div className="text-muted">{subtitle}</div>
              </div>
            </div>
            <div className="">{children}</div>
          </div>
        </NativeQueriesFeatureFlag>
      </LimitedFeatureWrapper>
    </div>
  );
};
