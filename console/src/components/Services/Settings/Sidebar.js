import React from 'react';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import { Link } from 'react-router';
import CheckIcon from '../../Common/Icons/Check';
import CrossIcon from '../../Common/Icons/Cross';
import globals from '../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../constants';
import { getAdminSecret } from '../ApiExplorer/ApiRequest/utils';

import styles from '../../Common/TableCommon/Table.scss';

const Sidebar = ({ location, metadata }) => {
  const sectionsData = [];

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
    key: 'allowed-queries',
    link: '/settings/allowed-queries',
    dataTestVal: 'allowed-queries-link',
    title: 'Allowed Queries',
  });

  const adminSecret = getAdminSecret();

  if (adminSecret && globals.consoleMode !== CLI_CONSOLE_MODE) {
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

  const sections = [];

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
