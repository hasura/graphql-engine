import { FaArrowRight, FaCloud } from 'react-icons/fa';
import { Link } from 'react-router';
import styles from '../MetricsV1.module.scss';
import { PostgresSource } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';

const EventNameItem = ({ event }) => {
  return (
    <Link to={`/events/data/${event.name}/modify`}>
      <Flex
        className={`${styles['actionLinkLayout']} ${styles['dagBody']} ${styles['sm']} ${styles['cardLink']}`}
        align="center"
        gap="1"
      >
        {event.name}
        <FaArrowRight
          className={`${styles['pull_right']} ${styles['hoverArrow']}`}
          aria-hidden="true"
        />
      </Flex>
    </Link>
  );
};
const DBEvents = ({ source }: { source: PostgresSource }) => {
  const events = source.tables?.reduce((eventsInfo, tableInfo) => {
    if (
      typeof tableInfo === 'object' &&
      'event_triggers' in tableInfo &&
      tableInfo?.event_triggers
    ) {
      return [...eventsInfo, ...(tableInfo.event_triggers as any[])];
    }

    return eventsInfo;
  }, [] as any[]);

  const eventsCount = events['length'];
  return (
    eventsCount > 0 && (
      <ul className={styles['length']}>
        <li>
          <div className={`${styles['dagCard']} event`}>
            <div
              className={`${styles['dagHeaderOnly']} ${styles['flexMiddle']} `}
            >
              <Flex align="center" gap="1">
                <FaCloud className={styles['mr_xxs']} aria-hidden="true" />
                {eventsCount === 1
                  ? `${eventsCount} Event`
                  : `${eventsCount} Events`}
              </Flex>
            </div>
            {events.map((event) => (
              <EventNameItem event={event} key={event.name} />
            ))}
          </div>
        </li>
      </ul>
    )
  );
};

export default DBEvents;
