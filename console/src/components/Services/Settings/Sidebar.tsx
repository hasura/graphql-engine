/* eslint-disable no-underscore-dangle */
import React from 'react';
import { Link, RouteComponentProps } from 'react-router';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import CheckIcon from '../../Common/Icons/Check';
import CrossIcon from '../../Common/Icons/Cross';
import globals from '../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../constants';
import { getAdminSecret } from '../ApiExplorer/ApiRequest/utils';

import styles from '../../Common/TableCommon/Table.scss';

interface Metadata {
  inconsistentObjects: Record<string, unknown>[];
}

type SidebarProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
  metadata: Metadata;
};

type SectionDataKey = 'actions' | 'status' | 'allow-list' | 'logout' | 'about';

interface SectionData {
  key: SectionDataKey;
  link: string;
  dataTestVal: string;
  title: string | JSX.Element;
}

const Sidebar: React.FC<SidebarProps> = ({ location, metadata }) => {
  const sectionsData: SectionData[] = [];

  sectionsData.push({
    key: 'actions',
    link: '/settings/metadata-actions',
    dataTestVal: 'metadata-actions-link',
    title: 'Metadata Actions',
  });

  const consistentIcon =
    metadata.inconsistentObjects.length === 0 ? <CheckIcon /> : <CrossIcon />;

  sectionsData.push({
    key: 'status',
    link: '/settings/metadata-status',
    dataTestVal: 'metadata-status-link',
    title: (
      <div className={styles.display_flex}>
        Metadata Status
        <span className={styles.add_mar_left}>{consistentIcon}</span>
      </div>
    ),
  });

  sectionsData.push({
    key: 'allow-list',
    link: '/settings/allow-list',
    dataTestVal: 'allow-list-link',
    title: 'Allow List',
  });

  const adminSecret = getAdminSecret();

  // userRole is only present on team console
  if (
    adminSecret &&
    globals.consoleMode !== CLI_CONSOLE_MODE &&
    (!window.__env.userRole || !window.location.host.includes('cloud'))
  ) {
    sectionsData.push({
      key: 'logout',
      link: '/settings/logout',
      dataTestVal: 'logout-page-link',
      title: 'Logout (clear admin-secret)',
    });
  }

  sectionsData.push({
    key: 'about',
    link: '/settings/about',
    dataTestVal: 'about-link',
    title: 'About',
  });

  const currentLocation = location.pathname;

  const sections: JSX.Element[] = [];

  sectionsData.forEach(section => {
    sections.push(
      <li
        role="presentation"
        key={section.key}
        className={currentLocation.includes(section.link) ? styles.active : ''}
      >
        <Link
          className={styles.linkBorder}
          to={section.link}
          data-test={section.dataTestVal}
        >
          {section.title}
        </Link>
      </li>
    );
  });

  const content = <ul>{sections}</ul>;

  return <LeftContainer>{content}</LeftContainer>;
};

export default Sidebar;
