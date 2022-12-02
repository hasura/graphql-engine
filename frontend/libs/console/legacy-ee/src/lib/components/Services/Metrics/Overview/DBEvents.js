import React from 'react';
import { FaArrowRight, FaCloud, FaSearch } from 'react-icons/fa';
import { Link } from 'react-router';
import styles from '../MetricsV1.module.scss';

const EventNameItem = ({ event }) => {
  return (
    <Link to={`/events/data/${event.name}/modify`}>
      <div
        className={`${styles.actionLinkLayout} ${styles.dagBody} ${styles.sm} ${styles.cardLink}`}
      >
        {event.name}
        <FaArrowRight
          className={`${styles.pull_right} ${styles.hoverArrow}`}
          aria-hidden="true"
        />
      </div>
    </Link>
  );
};
const DBEvents = ({ source: { tables = [] } }) => {
  const events = tables?.reduce((eventsInfo, tableInfo) => {
    if (
      typeof tableInfo === 'object' &&
      (tableInfo?.event_triggers || tableInfo.hasOwnProperty('event_triggers'))
    ) {
      return [...eventsInfo, ...tableInfo.event_triggers];
    }
    return eventsInfo;
  }, []);
  const eventsCount = events.length;
  return (
    eventsCount > 0 && (
      <ul className={styles.ul_pad_remove}>
        <li>
          <div className={`${styles.dagCard} event`}>
            <div className={`${styles.dagHeaderOnly} ${styles.flexMiddle} `}>
              <p className={`${styles.strong} ${styles.mr_xs}`}>
                <FaCloud className={styles.mr_xxs} aria-hidden="true" />
                {eventsCount === 1
                  ? `${eventsCount} Event`
                  : `${eventsCount} Events`}
              </p>
              <FaSearch
                className={`${styles.muted} ${styles.search}`}
                aria-hidden="true"
              />
            </div>
            {events.map(event => (
              <EventNameItem event={event} key={event.name} />
            ))}
          </div>
        </li>
      </ul>
    )
  );
};
export default DBEvents;
