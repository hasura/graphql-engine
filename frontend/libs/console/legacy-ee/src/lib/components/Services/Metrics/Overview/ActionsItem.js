import React from 'react';
import { FaArrowRight, FaCogs, FaSearch } from 'react-icons/fa';
import { Link } from 'react-router';
import styles from '../MetricsV1.module.scss';

const ActionNameItem = ({ action }) => {
  return (
    <Link to={`/actions/manage/${action.name}/modify`}>
      <div
        className={`${styles.actionLinkLayout} ${styles.dagBody} ${styles.sm} ${styles.cardLink}`}
      >
        {action.name}
        <FaArrowRight
          className={`${styles.pull_right} ${styles.hoverArrow}`}
          aria-hidden="true"
        />
      </div>
    </Link>
  );
};

const ActionsItem = ({ actions = [] }) => {
  if (!Array.isArray(actions) || !actions.length) {
    return null;
  }
  const count = actions.length;
  return (
    <li>
      <div className={`${styles.dagCard} action`}>
        <div className={`${styles.dagHeaderOnly} ${styles.flexMiddle} `}>
          <p className={`${styles.strong} ${styles.mr_xs}`}>
            <FaCogs className={styles.mr_xxs} aria-hidden="true" />
            {count === 1 ? `${count} Action` : `${count} Actions`}
          </p>
          <FaSearch
            className={`${styles.muted} ${styles.search}`}
            aria-hidden="true"
          />
        </div>
        {actions.map(action => (
          <ActionNameItem action={action} key={action.name} />
        ))}
      </div>
    </li>
  );
};
export default ActionsItem;
