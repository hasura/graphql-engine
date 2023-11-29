/* eslint-disable no-underscore-dangle */
import React from 'react';
import { RouteComponentProps } from 'react-router';
import { useServerConfig } from '../../../hooks';
import { useMetadata } from '../../../features/MetadataAPI';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import globals from '../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../constants';
import { getAdminSecret } from '../ApiExplorer/ApiRequest/utils';
import {
  NavigationSidebar,
  NavigationSidebarProps,
  NavigationSidebarSection,
} from '../../../new-components/NavigationSidebar';

import { useEELiteAccess } from '../../../features/EETrial';
import { getQueryResponseCachingRoute } from '../../../utils/routeUtils';
import { isOpenTelemetrySupported } from '../../../utils/proConsole';

export interface Metadata {
  inconsistentObjects: Record<string, unknown>[];
  inconsistentInheritedRoles: Record<string, unknown>[];
}

type SidebarProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
  metadata: Metadata;
};

type SectionDataKey =
  | 'metadata'
  | 'security'
  | 'monitoring'
  | 'performance'
  | 'about';

const Sidebar: React.FC<SidebarProps> = ({ location, metadata }) => {
  const eeLiteAccess = useEELiteAccess(globals);

  const sectionsData: Partial<
    Record<SectionDataKey, NavigationSidebarSection>
  > = {};
  sectionsData.metadata = {
    key: 'metadata',
    label: 'Metadata',
    items: [],
  };
  sectionsData.metadata.items.push({
    key: 'actions',
    label: 'Metadata Actions',
    route: '/settings/metadata-actions',
    dataTestVal: 'metadata-actions-link',
  });

  sectionsData.metadata.items.push({
    key: 'status',
    label: 'Metadata Status',
    route: '/settings/metadata-status',
    status:
      metadata.inconsistentObjects.length === 0 &&
      metadata.inconsistentInheritedRoles.length === 0
        ? 'enabled'
        : 'error',
    dataTestVal: 'metadata-status-link',
  });

  sectionsData.security = {
    key: 'security',
    label: 'Security',
    items: [],
  };

  sectionsData.security.items.push({
    key: 'allow-list',
    label: 'Allow List',
    route: '/api/allow-list',
    dataTestVal: 'allow-list-link',
  });

  const adminSecret = getAdminSecret();

  if (
    adminSecret &&
    globals.consoleMode !== CLI_CONSOLE_MODE &&
    globals.consoleType !== 'cloud'
  ) {
    sectionsData.security.items.push({
      key: 'logout',
      label: 'Logout (clear admin-secret)',
      route: '/settings/logout',
      dataTestVal: 'logout-page-link',
    });
  }

  sectionsData.security.items.push({
    key: 'inherited-roles',
    label: 'Inherited Roles',
    route: '/settings/inherited-roles',
    dataTestVal: 'inherited-roles-link',
  });

  sectionsData.security.items.push({
    key: 'insecure-domain',
    label: 'Insecure TLS Allow List',
    route: '/settings/insecure-domain',
    dataTestVal: 'insecure-domain-link',
  });

  const { data: openTelemetry } = useMetadata(m => m.metadata.opentelemetry);
  const { data: configData, isLoading, isError } = useServerConfig();

  if (eeLiteAccess.access !== 'forbidden') {
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
  }

  if (isOpenTelemetrySupported(window.__env)) {
    if (!sectionsData.monitoring) {
      sectionsData.monitoring = {
        key: 'monitoring',
        label: 'Monitoring & observability',
        items: [],
      };
    }

    sectionsData.monitoring.items.push({
      key: 'opentelemetry-settings',
      label: 'OpenTelemetry Exporter (Beta)',
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

  if (eeLiteAccess.access !== 'forbidden') {
    sectionsData.security.items.push({
      key: 'multiple-admin-secrets',
      label: 'Multiple admin secrets',
      route: '/settings/multiple-admin-secrets',
      dataTestVal: 'multiple-admin-secrets',
    });

    sectionsData.security.items.push({
      key: 'multiple-jwt-secrets',
      label: 'Multiple jwt secrets',
      route: '/settings/multiple-jwt-secrets',
      dataTestVal: 'multiple-jwt-secrets',
    });

    // sectionsData.security.items.push({
    //   key: 'single-sign-on',
    //   label: 'Single Sign On',
    //   route: '/settings/single-sign-on',
    //   dataTestVal: 'single-sign-on',
    // });

    sectionsData.performance = {
      key: 'performance',
      label: 'Performance',
      items: [
        {
          key: 'query-response-caching',
          label: 'Query Response Caching',
          // // TODO: Figure out the disabled/enabled logic
          // status:
          //   licenseInfo?.status !== 'active'
          //     ? 'disabled'
          //     : isLoading
          //     ? 'loading'
          //     : isError
          //     ? 'error'
          //     : 'enabled',
          route: getQueryResponseCachingRoute(),
          dataTestVal: 'query-response-caching',
        },
      ],
    };
  }

  sectionsData.about = {
    key: 'about',
    label: 'About',
    items: [],
  };

  sectionsData.about.items.push({
    key: 'feature-flags',
    label: 'Feature Flags',
    route: '/settings/feature-flags',
    dataTestVal: 'feature-flags-link',
  });

  sectionsData.about.items.push({
    key: 'about',
    label: 'About',
    route: '/settings/about',
    dataTestVal: 'about-link',
  });

  const sections: NavigationSidebarProps['sections'] =
    Object.values(sectionsData);

  return (
    <LeftContainer>
      <NavigationSidebar location={location} sections={sections} />
    </LeftContainer>
  );
};

export default Sidebar;
