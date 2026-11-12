import { FaCloud } from 'react-icons/fa';
import styles from '../MetricsV1.module.scss';
import { PostgresSource } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';

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
                <FaCloud aria-hidden="true" />
                <Text weight="bold">
                  {eventsCount === 1
                    ? `${eventsCount} Event`
                    : `${eventsCount} Events`}
                </Text>
              </Flex>
            </div>
          </div>
        </li>
      </ul>
    )
  );
};

export default DBEvents;
