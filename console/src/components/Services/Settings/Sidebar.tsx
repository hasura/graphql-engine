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
import {
  checkFeatureSupport,
  INSECURE_TLS_ALLOW_LIST,
} from '../../../helpers/versionUtils';

interface Metadata {
  inconsistentObjects: Record<string, unknown>[];
  inconsistentInheritedRoles: Record<string, unknown>[];
}

type SidebarProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
  metadata: Metadata;
};

type SectionDataKey =
  | 'actions'
  | 'status'
  | 'allow-list'
  | 'logout'
  | 'about'
  | 'inherited-roles'
  | 'insecure-domain';

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
    metadata.inconsistentObjects.length === 0 &&
    metadata.inconsistentInheritedRoles.length === 0 ? (
      <CheckIcon />
    ) : (
      <CrossIcon />
    );

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

  if (
    adminSecret &&
    globals.consoleMode !== CLI_CONSOLE_MODE &&
    globals.consoleType !== 'cloud'
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

  sectionsData.push({
    key: 'inherited-roles',
    link: '/settings/inherited-roles',
    dataTestVal: 'inherited-roles-link',
    title: 'Inherited Roles',
  });

  if (checkFeatureSupport(INSECURE_TLS_ALLOW_LIST))
    sectionsData.push({
      key: 'insecure-domain',
      link: '/settings/insecure-domain',
      dataTestVal: 'insecure-domain-link',
      title: 'Insecure TLS Allow List',
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
