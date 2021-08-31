import React from 'react';
import { Link, RouteComponentProps } from 'react-router';

import styles from './ApiExplorer.scss';

type TopNavProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
};

const TopNav: React.FC<TopNavProps> = ({ location }) => {
  const sectionsData = [
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
  ];

  // eslint-disable-next-line no-underscore-dangle
  if (window.__env.consoleId || window.__env.projectID) {
    sectionsData.push({
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
    <div className={styles.topNavContainer}>
      {sectionsData.map(section => (
        <div role="presentation" key={section.key}>
          <Link
            to={section.link}
            data-test={section.dataTestVal}
            className={`${styles.sectionLink} ${
              isActive(section.link) ? styles.sectionLinkActive : ''
            }`}
          >
            {section.title}
          </Link>
        </div>
      ))}
    </div>
  );
};

export default TopNav;
