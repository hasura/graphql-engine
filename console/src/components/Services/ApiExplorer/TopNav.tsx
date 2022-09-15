import React from 'react';
import { Link, RouteComponentProps } from 'react-router';
import { canAccessSecuritySettings } from '@/utils/permissions';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '@/features/FeatureFlags';

type TopNavProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
};

const TopNav: React.FC<TopNavProps> = ({ location }) => {
  const { enabled: allowListEnabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.allowListId
  );

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
      ...(allowListEnabled
        ? [
            {
              key: 'allow-list',
              link: '/api/allow-list',
              dataTestVal: 'allow-list',
              title: 'Allow List',
            },
          ]
        : []),
    ],
  ];

  if (canAccessSecuritySettings()) {
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
            >
              <Link
                to={section.link}
                data-test={section.dataTestVal}
                className="text-gray-600 font-semibold no-underline hover:text-gray-600 hover:no-underline focus:text-gray-600 focus:no-underline"
              >
                {section.title}
              </Link>
            </div>
          ))
        )}
      </div>
    </div>
  );
};

export default TopNav;
