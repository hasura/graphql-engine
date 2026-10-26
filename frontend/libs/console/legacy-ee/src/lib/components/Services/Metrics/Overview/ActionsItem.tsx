import { FaCogs } from 'react-icons/fa';
import styles from '../MetricsV1.module.scss';
import { Flex } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';

const ActionsItem = ({ actions = [] }: { actions: string[] }) => {
  if (!Array.isArray(actions) || !actions.length) {
    return null;
  }
  const count = actions.length;
  return (
    <li>
      <div className={`${styles['dagCard']} action`}>
        <div className={`${styles['dagHeaderOnly']} ${styles['flexMiddle']} `}>
          <Flex align="center" gap="1">
            <FaCogs />
            <Text weight="bold">
              {count === 1 ? `${count} Action` : `${count} Actions`}
            </Text>
          </Flex>
        </div>
      </div>
    </li>
  );
};

export default ActionsItem;
