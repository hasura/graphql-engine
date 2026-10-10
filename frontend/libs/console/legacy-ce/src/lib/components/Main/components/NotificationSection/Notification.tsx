import React from 'react';
import { Flex } from '@radix-ui/themes';
import { FaArrowRight } from 'react-icons/fa';
import type { ConsoleNotification, ConsoleScope } from '@hasura/metadata/api';
import styles from '../../Main.module.scss';
import { getDateString, toShowNotification } from './utils';
import { LegacyBadge } from '@hasura/shared/ui';

interface UpdateProps extends ConsoleNotification {
  onClick?: (id?: number) => void;
  is_read?: boolean;
  consoleScope: ConsoleScope;
  latestVersion?: string;
  stable?: boolean;
  isSpecial?: boolean;
  children?: React.ReactNode;
}

const Notification: React.FC<UpdateProps> = ({
  subject,
  content,
  type,
  is_active = true,
  onClick,
  is_read,
  isSpecial,
  ...props
}) => {
  const [currentReadState, updateReadState] = React.useState(is_read);

  React.useEffect(() => {
    if (is_read) {
      updateReadState(true);
      return;
    }
    updateReadState(false);
  }, [is_read]);

  const onClickNotification = () => {
    if (!currentReadState) {
      updateReadState(true);
    }
    if (onClick) {
      onClick(props.id);
    }
  };

  if (!is_active) {
    return null;
  }

  if (!toShowNotification(props.consoleScope, props.scope, isSpecial)) {
    return null;
  }

  const isUpdateNotification =
    type === 'beta update' || type === 'version update';
  const updateContainerClass = isUpdateNotification
    ? styles.updateStyleBox
    : styles.updateBox;

  return (
    <div
      className={`${updateContainerClass} ${
        !currentReadState ? styles.unread : styles.read
      }`}
      onClick={onClickNotification}
    >
      {!isUpdateNotification ? (
        <div
          className={`${styles.unreadDot} ${
            currentReadState ? styles.hideDot : ''
          }`}
        />
      ) : (
        <span
          className={`${styles.unreadStar} ${
            currentReadState ? styles.hideStar : ''
          }`}
          role="img"
          aria-label="star emoji"
        >
          ⭐️
        </span>
      )}
      <Flex className="w-full">
        <Flex
          direction="column"
          className="w-4/5"
          style={{
            paddingLeft: 32,
            paddingRight: 25,
            paddingTop: 6,
            paddingBottom: 6,
          }}
        >
          <p className="m-0 pb-1 text-[10px] font-bold leading-[12px] text-[#717780]">
            {props?.start_date ? getDateString(props.start_date) : null}
          </p>
          <h4 className="mb-3 text-sm font-bold leading-[153%] text-[#1B2738]">
            {subject}
          </h4>
          <p className="text-[15px] font-normal">
            {content}
            <br />
            {props?.children ? props.children : null}
          </p>
        </Flex>
        <Flex
          direction="column"
          align="end"
          justify="end"
          className="w-1/5 mr-6 mt-2.5"
        >
          <div className={`${styles.splitHalf}`}>
            {type ? <LegacyBadge type={type} style={{ marginTop: 8 }} /> : null}
          </div>
          <div className={`${styles.splitHalf} ${styles.linkPosition}`}>
            {props.external_link ? (
              <div className={styles.linkContainer}>
                <a
                  href={props.external_link}
                  className={styles.notificationExternalLink}
                  onClick={onClickNotification}
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  <FaArrowRight className={styles.linkArrow} />
                </a>
              </div>
            ) : null}
          </div>
        </Flex>
      </Flex>
    </div>
  );
};

export default Notification;
