import React from 'react';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import { Link } from 'react-router';
import styles from '../../Common/TableCommon/Table.scss';
import CheckIcon from '../../Common/Icons/Check';
import CrossIcon from '../../Common/Icons/Cross';

const Sidebar = ({ location, semverChecks, metadata }) => {
  const currentLocation = location.pathname;

  const sections = [];

  if (semverChecks.supportMetadata) {
    sections.push(
      <li
        role="presentation"
        className={
          currentLocation.includes('metadata/actions') ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={'/metadata/actions'}
          data-test="metadata-actions-link"
        >
          Metadata Actions
        </Link>
      </li>
    );
  }

  if (semverChecks.supportInconsistentMetadata) {
    const consistentIcon =
      metadata.inconsistentObjects.length === 0 ? <CheckIcon /> : <CrossIcon />;

    sections.push(
      <li
        role="presentation"
        className={
          currentLocation.includes('metadata/status') ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={'/metadata/status'}
          data-test="metadata-status-link"
        >
          <div className={styles.display_flex}>
            Metadata Status
            <span className={styles.add_mar_left}>{consistentIcon}</span>
          </div>
        </Link>
      </li>
    );
  }

  if (semverChecks.supportQueryWhitelist) {
    sections.push(
      <li
        role="presentation"
        className={
          currentLocation.includes('metadata/whitelist-queries')
            ? styles.active
            : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={'/metadata/whitelist-queries'}
          data-test="metadata-whitelist-query-link"
        >
          Whitelist Queries
        </Link>
      </li>
    );
  }

  const content = <ul>{sections}</ul>;

  return <LeftContainer>{content}</LeftContainer>;
};

export default Sidebar;
