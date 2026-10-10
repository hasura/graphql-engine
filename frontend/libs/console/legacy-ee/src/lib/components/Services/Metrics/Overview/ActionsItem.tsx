import { FaArrowRight, FaCogs } from 'react-icons/fa';
import { Link } from 'react-router';
import styles from '../MetricsV1.module.scss';
import { Flex } from '@radix-ui/themes';

const ActionNameItem = ({ action }) => {
  return (
    <Link to={`/actions/manage/${action.name}/modify`}>
      <Flex
        className={`${styles['actionLinkLayout']} ${styles['dagBody']} ${styles['sm']} ${styles['cardLink']}`}
        align="center"
        gap="1"
      >
        {action.name}
        <FaArrowRight
          className={`${styles['pull_right']} ${styles['hoverArrow']}`}
          aria-hidden="true"
        />
      </Flex>
    </Link>
  );
};

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
            {count === 1 ? `${count} Action` : `${count} Actions`}
          </Flex>
        </div>
        {actions.map((action) => (
          <ActionNameItem action={action} key={action['name']} />
        ))}
      </div>
    </li>
  );
};

export default ActionsItem;
