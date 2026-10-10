import React from 'react';
import { CLI_CONSOLE_MODE } from '@hasura/shared/types';
import {
  NavigationSidebar,
  NavigationSidebarProps,
  NavigationSidebarSection,
} from './NavigationSidebar';
import { useEELiteAccess } from '../../../features/EETrial';
import { isOpenTelemetrySupported } from '@hasura/shared/utils';
import {
  useInconsistentMetadata,
  useMetadata,
  useServerConfig,
} from '@hasura/metadata/api';
import { useAppContext, useAuthContext } from '@hasura/shared/context';
import { LeftContainer } from '@hasura/shared/ui';

type SectionDataKey =
  'metadata' | 'security' | 'monitoring' | 'performance' | 'about';

const Sidebar: React.FC = () => {
  const { envVars } = useAppContext();
  const eeLiteAccess = useEELiteAccess();
  const { authType } = useAuthContext();
  const { data: inconsistentMetadata } = useInconsistentMetadata();

  const sectionsData: Partial<
    Record<SectionDataKey, NavigationSidebarSection>
  > = {
    metadata: {
      key: 'metadata',
      label: 'Metadata',
      items: [
        {
          key: 'actions',
          label: 'Metadata Actions',
          route: '/settings/metadata-actions',
          dataTestVal: 'metadata-actions-link',
        },
        {
          key: 'status',
          label: 'Metadata Status',
          route: '/settings/metadata-status',
          status:
            !inconsistentMetadata || inconsistentMetadata.is_consistent
              ? 'enabled'
              : 'error',
          dataTestVal: 'metadata-status-link',
        },
      ],
    },
    security: {
      key: 'security',
      label: 'Security',
      items: [
        {
          key: 'allow-list',
          label: 'Allow List',
          route: '/api/allow-list',
          dataTestVal: 'allow-list-link',
        },
      ],
    },
  };

  if (
    authType === 'admin-secret' &&
    envVars.consoleMode !== CLI_CONSOLE_MODE &&
    envVars.consoleType !== 'cloud'
  ) {
    sectionsData.security?.items.push({
      key: 'logout',
      label: 'Logout (clear admin-secret)',
      route: '/settings/logout',
      dataTestVal: 'logout-page-link',
    });
  }

  sectionsData.security?.items.push(
    {
      key: 'inherited-roles',
      label: 'Inherited Roles',
      route: '/settings/inherited-roles',
      dataTestVal: 'inherited-roles-link',
    },
    {
      key: 'insecure-domain',
      label: 'Insecure TLS Allow List',
      route: '/settings/insecure-domain',
      dataTestVal: 'insecure-domain-link',
    },
  );

  const { data: openTelemetry } = useMetadata((m) => m.metadata.opentelemetry);
  const { data: configData, isLoading, isError } = useServerConfig();

  if (isOpenTelemetrySupported(envVars)) {
    sectionsData.monitoring = {
      key: 'monitoring',
      label: 'Monitoring & observability',
      items: [],
    };
    sectionsData.monitoring.items.push({
      key: 'prometheus-settings',
      label: 'Prometheus Metrics',
      status:
        eeLiteAccess.access !== 'active'
          ? 'disabled'
          : isLoading
            ? 'loading'
            : isError
              ? 'error'
              : configData?.is_prometheus_metrics_enabled
                ? 'enabled'
                : 'disabled',
      route: '/settings/prometheus-settings',
      dataTestVal: 'prometheus-settings-link',
    });

    sectionsData.monitoring.items.push({
      key: 'opentelemetry-settings',
      label: 'OpenTelemetry Exporter',
      status:
        eeLiteAccess.access !== 'active' &&
        !isOpenTelemetrySupported(window.__env)
          ? 'disabled'
          : !openTelemetry
            ? 'none'
            : openTelemetry.status === 'enabled'
              ? 'enabled'
              : 'disabled',
      route: '/settings/opentelemetry',
      dataTestVal: 'opentelemetry-settings-link',
    });
  }

  if (
    envVars.consoleType === 'cloud' ||
    envVars.consoleType === 'pro' ||
    eeLiteAccess.access !== 'forbidden'
  ) {
    sectionsData.security?.items.push(
      {
        key: 'multiple-admin-secrets',
        label: 'Multiple admin secrets',
        route: '/settings/multiple-admin-secrets',
        dataTestVal: 'multiple-admin-secrets',
      },
      {
        key: 'multiple-jwt-secrets',
        label: 'Multiple jwt secrets',
        route: '/settings/multiple-jwt-secrets',
        dataTestVal: 'multiple-jwt-secrets',
      },
    );

    sectionsData.performance = {
      key: 'performance',
      label: 'Performance',
      items: [
        {
          key: 'query-response-caching',
          label: 'Query Response Caching',
          route: '/settings/query-response-caching',
          dataTestVal: 'query-response-caching',
        },
      ],
    };
  }

  sectionsData.about = {
    key: 'about',
    label: 'About',
    items: [
      // {
      //   key: 'feature-flags',
      //   label: 'Feature Flags',
      //   route: '/settings/feature-flags',
      //   dataTestVal: 'feature-flags-link',
      // },
      {
        key: 'about',
        label: 'About',
        route: '/settings/about',
        dataTestVal: 'about-link',
      },
    ],
  };

  const sections: NavigationSidebarProps['sections'] =
    Object.values(sectionsData);

  return (
    <LeftContainer>
      <NavigationSidebar sections={sections} />
    </LeftContainer>
  );
};

export default Sidebar;
