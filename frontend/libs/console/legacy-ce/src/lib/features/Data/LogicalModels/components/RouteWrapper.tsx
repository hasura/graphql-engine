import React from 'react';
import { Breadcrumbs, LearnMoreLink, Text } from '@hasura/shared/ui';
import { NATIVE_QUERY_ROUTE_DETAIL } from '../constants';
import { injectRouteDetails, pathsToBreadcrumbs } from './route-wrapper-utils';
import { Flex, Heading } from '@radix-ui/themes';
import { useNavigate } from 'react-router';

export type RouteWrapperProps = {
  children?: React.ReactNode;
  route: keyof typeof NATIVE_QUERY_ROUTE_DETAIL;
  itemSourceName?: string;
  itemName?: string;
  itemTabName?: string;
  subtitle?: string;
};

export const RouteWrapper: React.FC<RouteWrapperProps> = (props) => {
  const { children, route, subtitle: subtitleOverride } = props;

  const paths = route?.split('/').filter(Boolean);

  const { title, subtitle, docLink } = NATIVE_QUERY_ROUTE_DETAIL[route];

  const push = useNavigate();

  return (
    <div className="py-6 px-4 w-full">
      <Flex direction="column" gap="4">
        <Breadcrumbs items={pathsToBreadcrumbs(paths, props, push)} />
        <Flex justify="between" className="w-full px-2">
          <div className="mb-2">
            <Heading size="4">{injectRouteDetails(title, props)}</Heading>
            <Text>
              {subtitleOverride ?? subtitle}{' '}
              {docLink && <LearnMoreLink href={docLink} />}
            </Text>
          </div>
        </Flex>
      </Flex>
      <div>{children}</div>
    </div>
  );
};
