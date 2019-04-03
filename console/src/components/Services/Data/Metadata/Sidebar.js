import React from 'react';
import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import { Link } from 'react-router';
import globals from '../../../../Globals';
import styles from '../../../Common/TableCommon/Table.scss';
import CheckIcon from '../../../Common/Icons/Check';
import CrossIcon from '../../../Common/Icons/Cross';

const sectionPrefix = globals.urlPrefix + '/metadata';

const Sidebar = ({
  location,
  supportMetadata,
  supportInconsistentMetadata,
  metadata,
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
          Metadata Options
        </Link>
      </li>
    );
  }
  if (supportInconsistentMetadata) {
    let consistentIcon = <CheckIcon className={styles.add_mar_left_small} />;
    if (metadata.inconsistentObjects.length > 0) {
      consistentIcon = <CrossIcon className={styles.add_mar_left_small} />;
    }
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
          <div className={styles.display_flex}>
            Metadata Status
            {consistentIcon}
          </div>
        </Link>
      </li>
    );
  }
  const content = <ul>{sections}</ul>;
  return <LeftContainer>{content}</LeftContainer>;
};

export default Sidebar;
