import React from 'react';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import { Link } from 'react-router';
import styles from '../../Common/TableCommon/Table.scss';
import CheckIcon from '../../Common/Icons/Check';
import CrossIcon from '../../Common/Icons/Cross';

const Sidebar = ({ location, metadata }) => {
  const sectionsData = [];

  sectionsData.push({
    key: 'actions',
    link: '/metadata/actions',
    dataTestVal: 'metadata-actions-link',
    title: 'Metadata Actions',
  });

  const consistentIcon =
    metadata.inconsistentObjects.length === 0 ? <CheckIcon /> : <CrossIcon />;

  sectionsData.push({
    key: 'status',
    link: '/metadata/status',
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
    link: '/metadata/allowed-queries',
    dataTestVal: 'metadata-allowed-queries-link',
    title: 'Allowed Queries',
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
