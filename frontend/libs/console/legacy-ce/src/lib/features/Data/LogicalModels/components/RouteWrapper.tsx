import startCase from 'lodash/startCase';
import React, { ReactNode } from 'react';
import { Breadcrumbs } from '../../../../new-components/Breadcrumbs';
import { LimitedFeatureWrapper } from '../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import {
  useEnvironmentState,
  usePushRoute,
} from '../../../ConnectDBRedesign/hooks';
import { NATIVE_QUERY_ROUTES } from '../constants';

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
  const { consoleType } = useEnvironmentState();

  return (
    <div className="py-md px-md w-full">
      <LimitedFeatureWrapper
        title="Looking to add Native Queries?"
        id="native-queries"
        description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
        override={consoleType === 'oss'}
      >
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
      </LimitedFeatureWrapper>
    </div>
  );
};
