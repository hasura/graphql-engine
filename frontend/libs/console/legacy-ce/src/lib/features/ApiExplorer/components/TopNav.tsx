import React from 'react';
import { Link, useLocation } from 'react-router';
import { isProConsole, isCloudConsole } from '@hasura/shared/utils';
import { useEELiteAccess } from '../../EETrial';
import { IconTooltip, TabNav, Text } from '@hasura/shared/ui';
import { sendTelemetryEvent } from '../../../telemetry';
import { getLSItem, setLSItem } from '@hasura/shared/utils';
import {
  BreakingChangesColor,
  BreakingChangesTooltipMessage,
  DangerousChangesColor,
  DangerousChangesTooltipMessage,
  DefaultToopTipMessage,
  SafeChangesColor,
  SafeChangesTooltipMessage,
} from '../../SchemaRegistry/constants';
import { useGetSchemaRegistryNotificationColor } from '../../SchemaRegistry/hooks/useGetSchemaRegistryNotificationColor';
import { LS_KEYS } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { FaExclamationCircle } from 'react-icons/fa';
import { FaRegMap } from 'react-icons/fa6';

const TopNav = () => {
  const location = useLocation();
  const { envVars } = useAppContext();
  const { access: eeLiteAccess } = useEELiteAccess();

  const sectionsData = [
    [
      {
        key: 'graphiql',
        link: '/api/api-explorer',
        dataTestVal: 'graphiql-explorer-link',
        title: 'GraphiQL',
      },
      {
        key: 'rest',
        link: '/api/rest',
        dataTestVal: 'rest-explorer-link',
        title: 'REST',
      },
    ],
    [
      {
        key: 'allow-list',
        link: '/api/allow-list',
        dataTestVal: 'allow-list',
        title: 'Allow List',
      },
    ],
  ];

  if (isProConsole(envVars) || eeLiteAccess !== 'forbidden') {
    sectionsData[1].push({
      key: 'security',
      link: '/api/security/api_limits',
      dataTestVal: 'security-explorer-link',
      title: 'Security',
    });

    if (
      isCloudConsole(envVars) &&
      (envVars.userRole === 'admin' || envVars.userRole === 'owner')
    ) {
      sectionsData[0].push({
        key: 'schema-registry',
        link: '/api/schema-registry',
        dataTestVal: 'schema-registry-link',
        title: 'Schema Registry',
      });
    }
  }

  const isActive = (link: string): boolean => {
    if (location.pathname === '' || location.pathname === '/') {
      return link.includes('api-explorer');
    }
    return location.pathname.includes(link);
  };

  const projectID = envVars.projectID || '';
  const fetchSchemaRegistryNotificationData =
    useGetSchemaRegistryNotificationColor(projectID);

  let color = '';
  let tooltipMessage = DefaultToopTipMessage;
  let change_recorded_at = '';
  let showNotifications = false;

  if (fetchSchemaRegistryNotificationData.kind === 'success') {
    const data =
      fetchSchemaRegistryNotificationData?.response?.schema_registry_dumps_v2 ||
      [];
    if (
      data.length &&
      data[0].diff_with_previous_schema &&
      data[0].diff_with_previous_schema[0] &&
      data[0].diff_with_previous_schema[0].schema_diff_data &&
      data[0].change_recorded_at
    ) {
      const changes = data[0].diff_with_previous_schema[0].schema_diff_data;
      // Check if there's a change with a criticality level of "BREAKING"
      const hasBreakingChange = changes.some(
        (change) =>
          change.criticality && change.criticality.level === 'BREAKING',
      );
      const hasDangerousChange = changes.some(
        (change) =>
          change.criticality && change.criticality.level === 'DANGEROUS',
      );
      const last_viewed_change = getLSItem(LS_KEYS.lastViewedSchemaChange);

      if (
        (!last_viewed_change ||
          last_viewed_change < data[0].change_recorded_at) &&
        changes
      ) {
        if (hasBreakingChange) {
          color = BreakingChangesColor;
          tooltipMessage = BreakingChangesTooltipMessage;
        } else if (hasDangerousChange) {
          //gold color instead of yellow to be more visible
          color = DangerousChangesColor;
          tooltipMessage = DangerousChangesTooltipMessage;
        } else {
          color = SafeChangesColor;
          tooltipMessage = SafeChangesTooltipMessage;
        }
        change_recorded_at = data[0].change_recorded_at;
        showNotifications = true;
      }
    }
  }

  return (
    <TabNav.Root>
      {sectionsData.map((group, groupIndex) =>
        group.map((section, sectionIndex) => (
          <TabNav.Link
            key={`${groupIndex}-${sectionIndex}`}
            asChild
            active={isActive(section.link)}
            onClick={() => {
              if (showNotifications) {
                setLSItem(LS_KEYS.lastViewedSchemaChange, change_recorded_at);
              }
              // Send Telemetry data for Schema Registry tab
              if (section.key === 'schema-registry') {
                sendTelemetryEvent({
                  type: 'CLICK_EVENT',
                  data: {
                    id: 'schema-registry-top-nav-tab',
                  },
                });
              }
            }}
          >
            <Link to={section.link} data-test={section.dataTestVal}>
              <Text weight="bold">{section.title}</Text>
              {section.key === 'schema-registry' && (
                <IconTooltip
                  icon={
                    color ? (
                      <FaExclamationCircle style={{ color }} />
                    ) : (
                      <FaRegMap />
                    )
                  }
                  message={tooltipMessage}
                />
              )}
            </Link>
          </TabNav.Link>
        )),
      )}
    </TabNav.Root>
  );
};

export default TopNav;
