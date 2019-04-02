import React from 'react';
import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import { Link } from 'react-router';
import globals from '../../../../Globals';
import styles from '../../../Common/TableCommon/Table.scss';

const sectionPrefix = globals.urlPrefix + '/metadata';

const Sidebar = ({
  location,
  supportMetadata,
  supportInconsistentMetadata,
}) => {
  const currentLocation = location.pathname;
  const sections = [];
  if (supportMetadata) {
    sections.push(
      <li
        role="presentation"
        className={
          currentLocation.includes('metadata/options') ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={sectionPrefix + '/options'}
          data-test="metadata-options-link"
        >
          Options
        </Link>
      </li>
    );
  }
  if (supportInconsistentMetadata) {
    sections.push(
      <li
        role="presentation"
        className={
          currentLocation.includes('metadata/status') ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={sectionPrefix + '/status'}
          data-test="metadata-status-link"
        >
          Status
        </Link>
      </li>
    );
  }
  const content = <ul>{sections}</ul>;
  return <LeftContainer>{content}</LeftContainer>;
};

export default Sidebar;
