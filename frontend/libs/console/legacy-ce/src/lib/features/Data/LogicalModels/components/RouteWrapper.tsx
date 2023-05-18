import Skeleton from 'react-loading-skeleton';
import { LimitedFeatureWrapper } from '../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { useServerConfig } from '../../../../hooks';
import { ReactNode } from 'react';
import { Breadcrumbs } from '../../../../new-components/Breadcrumbs';
import startCase from 'lodash/startCase';

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
  // eslint-disable-next-line react/jsx-no-useless-fragment
  return <>{children}</>;
}

type AllowedTabs =
  | 'logical-models'
  | 'native-queries'
  | 'stored-procedures'
  | 'create';
const getTitleAndSubtitle = (tabType: AllowedTabs) => {
  if (tabType === 'logical-models')
    return {
      title: 'Logical Models',
      subtitle:
        'Creating Logical Models in advance can help generate Native Queries faster',
    };
  if (tabType === 'native-queries')
    return {
      title: 'Native Queries',
      subtitle:
        'Access more queries and operators through SQL on your database',
    };
  if (tabType === 'stored-procedures')
    return {
      title: 'Stored Procedures',
      subtitle: 'Add support for stored procedures on SQL over a GraphQL API',
    };
  if (tabType === 'create')
    return {
      title: 'Create Native Query',
      subtitle:
        'Access more queries and operators through SQL on your database',
    };
  return {
    title: 'Not a valid path',
    subtitle: '',
  };
};

export const RouteWrapper = ({
  children,
  pathname,
  push,
}: {
  children: ReactNode;
  pathname: string | undefined;
  push?: (to: string) => void;
}) => {
  const paths = pathname?.split('/').filter(Boolean) ?? [];
  const tabType = paths[paths.length - 1] as AllowedTabs;
  const { title, subtitle } = getTitleAndSubtitle(tabType);
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
                  title: startCase(path),
                  onClick:
                    index === paths.length - 1
                      ? undefined
                      : () => {
                          push?.(`/${paths.slice(0, index + 1).join('/')}`);
                        },
                };
              })}
            />
            <div className="flex w-full justify-between px-2">
              <div className="mb-sm">
                <div className="text-xl font-bold mt-2">{title}</div>
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
