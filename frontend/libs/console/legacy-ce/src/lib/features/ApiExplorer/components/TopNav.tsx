import { Link, useLocation } from 'react-router';
import { isCloudConsole } from '@hasura/shared/utils';
import { TabNav, Text } from '@hasura/shared/ui';
import { sendTelemetryEvent } from '../../../telemetry';
import { useAppContext } from '@hasura/shared/context';
import SchemaRegistryBadge from './SchemaRegistryBadge';

const TopNav = () => {
  const location = useLocation();
  const { envVars } = useAppContext();

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
      {
        key: 'security',
        link: '/api/security/api_limits',
        dataTestVal: 'security-explorer-link',
        title: 'Security',
      },
    ],
  ];

  const canAccessSchemaRegistry =
    isCloudConsole(envVars) &&
    (envVars.userRole === 'admin' || envVars.userRole === 'owner');

  if (canAccessSchemaRegistry) {
    sectionsData[0].push({
      key: 'schema-registry',
      link: '/api/schema-registry',
      dataTestVal: 'schema-registry-link',
      title: 'Schema Registry',
    });
  }

  const isActive = (link: string): boolean => {
    if (location.pathname === '' || location.pathname === '/') {
      return link.includes('api-explorer');
    }
    return location.pathname.includes(link);
  };

  return (
    <TabNav.Root>
      {sectionsData.map((group, groupIndex) =>
        group.map((section, sectionIndex) => (
          <TabNav.Link
            key={`${groupIndex}-${sectionIndex}`}
            asChild
            active={isActive(section.link)}
            onClick={() => {
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
              {section.key === 'schema-registry' && <SchemaRegistryBadge />}
            </Link>
          </TabNav.Link>
        )),
      )}
    </TabNav.Root>
  );
};

export default TopNav;
