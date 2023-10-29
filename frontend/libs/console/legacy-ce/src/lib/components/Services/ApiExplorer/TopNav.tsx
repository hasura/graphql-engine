import React from 'react';
import { Link, RouteComponentProps } from 'react-router';
import { isProConsole } from '../../../utils/proConsole';
import { useEELiteAccess } from '../../../features/EETrial';
import globals from '../../../Globals';
import { IconTooltip } from '../../../new-components/Tooltip';
import { FaExclamationCircle, FaRegMap } from 'react-icons/fa';
import { sendTelemetryEvent } from '../../../telemetry';
import { useGetSchemaRegistryNotificationColor } from '../../../features/SchemaRegistry/hooks/useGetSchemaRegistryNotificationColor';
import { getLSItem, LS_KEYS, setLSItem } from '../../../utils';
import {
  BreakingChangesColor,
  BreakingChangesTooltipMessage,
  DangerousChangesColor,
  DangerousChangesTooltipMessage,
  DefaultToopTipMessage,
  SafeChangesColor,
  SafeChangesTooltipMessage,
} from '../../../features/SchemaRegistry/constants';

type TopNavProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
};

const TopNav: React.FC<TopNavProps> = ({ location }) => {
  const { access: eeLiteAccess } = useEELiteAccess(globals);

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
      {
        key: 'schema-registry',
        link: '/api/schema-registry',
        dataTestVal: 'schema-registry-link',
        title: 'Schema Registry',
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

  if (isProConsole(globals) || eeLiteAccess !== 'forbidden') {
    sectionsData[1].push({
      key: 'security',
      link: '/api/security/api_limits',
      dataTestVal: 'security-explorer-link',
      title: 'Security',
    });
  }

  const isActive = (link: string) => {
    if (location.pathname === '' || location.pathname === '/') {
      return link.includes('api-explorer');
    }
    return location.pathname.includes(link);
  };
  const projectID = globals.hasuraCloudProjectId || '';
  const fetchSchemaRegistryNotificationData =
    useGetSchemaRegistryNotificationColor(projectID);
  let color = '';
  let tooltipMessage = DefaultToopTipMessage;
  let change_recorded_at = '';
  let showNotifications = false;
  if (fetchSchemaRegistryNotificationData.kind === 'success') {
    const data =
      fetchSchemaRegistryNotificationData?.response
        ?.schema_registry_dumps_v2[0] || [];
    if (
      data &&
      data.diff_with_previous_schema[0] &&
      data.diff_with_previous_schema[0].schema_diff_data &&
      data.change_recorded_at
    ) {
      const changes = data.diff_with_previous_schema[0].schema_diff_data;
      // Check if there's a change with a criticality level of "BREAKING"
      const hasBreakingChange = changes.some(
        change => change.criticality && change.criticality.level === 'BREAKING'
      );
      const hasDangerousChange = changes.some(
        change => change.criticality && change.criticality.level === 'DANGEROUS'
      );
      const last_viewed_change = getLSItem(LS_KEYS.lastViewedSchemaChange);

      if (
        (!last_viewed_change || last_viewed_change < data.change_recorded_at) &&
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
        change_recorded_at = data.change_recorded_at;
        showNotifications = true;
      }
    }
  }
  return (
    <div className="flex justify-between items-center border-b border-gray-300 bg-white px-sm">
      <div className="flex px-1 w-full">
        {sectionsData.map((group, groupIndex) =>
          group.map((section, sectionIndex) => (
            <div
              role="presentation"
              className={`${
                groupIndex === 1 && sectionIndex === 0 ? 'ml-auto' : 'ml-4'
              } whitespace-nowrap font-medium pt-2 pb-1 px-2 border-b-4
        ${
          isActive(section.link)
            ? 'border-gray-300 hover:border-gray-300'
            : 'border-white hover:border-gray-100'
        }`}
              key={section.key}
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
              <Link
                to={section.link}
                data-test={section.dataTestVal}
                className="flex text-gray-600 font-semibold no-underline hover:text-gray-600 hover:no-underline focus:text-gray-600 focus:no-underline"
              >
                {section.title}
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
            </div>
          ))
        )}
      </div>
    </div>
  );
};

export default TopNav;
