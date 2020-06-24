import React from 'react';
import { connect } from 'react-redux';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import { ConsoleNotification } from './ConsoleNotification';
import styles from './Main.scss';
import PixelHeart from './images/components/PixelHeart';
import ConsoleLogo from './images/components/ConsoleLogo';
import { useOnClickOutside } from './utils';

const getDateString = (date: string | number | Date) => {
  return new Date(date).toLocaleString().split(', ')[0];
};

const Update: React.FC<ConsoleNotification> = ({
  subject,
  created_at,
  content,
  type,
  is_active = true,
  ...props
}) => {
  if (!is_active) {
    return null;
  }

  return (
    <Box>
      <Flex height={55} px="25px" pt="5px" justifyContent="space-between">
        <Flex justifyContent="space-between" bg="white">
          {type !== '' ? <Badge type={type} mr="12px" /> : null}
          <Heading as="h4" color="#1cd3c6" fontSize="16px">
            {subject}
          </Heading>
        </Flex>
        <Text color="grey" fontSize={13} fontWeight="bold">
          {created_at}
        </Text>
      </Flex>
      <Flex borderBottom="1px solid #f7f7f7">
        <Text fontSize={15} fontWeight="normal" px={15} py={4}>
          {content}
          {props.external_link ? 
          <a
            href={props.external_link}
            className={styles.notificationExternalLink}
          >
            Click here &rarr; 
          </a> : null}
        </Text>
      </Flex>
    </Box>
  );
};

type NotificationProps = {
  data: Array<ConsoleNotification>;
};

const Notifications = React.forwardRef<HTMLDivElement, NotificationProps>(
  ({ data }, ref) => (
    <Box
      className={`dropdown-menu ${styles.consoleNotificationPanel}`}
      ref={ref}
    >
      <Flex justifyContent="space-between" px={20} py={3}>
        <Heading as="h2" color="#000" fontSize="20px">
          Latest updates
          <ConsoleLogo className={styles.consoleLogoNotifications} width={20} />
        </Heading>
      </Flex>
      {data.length &&
        data.map(({ subject, created_at, content, is_active, ...props }) => (
          <Update
            key={props.id}
            subject={subject}
            created_at={getDateString(created_at)}
            content={content}
            type={props.type ? props.type : ''}
            is_active={is_active}
            {...props}
          />
        ))}
    </Box>
  )
);

type LoveSectionProps = {
  consoleNotifications: Array<ConsoleNotification>;
  toggleDropDown: (e: React.MouseEvent<HTMLDivElement>) => void;
  closeDropDown: () => void;
};

const LoveSection: React.FC<LoveSectionProps> = ({
  consoleNotifications,
  toggleDropDown,
  closeDropDown,
}) => {
  const dropDownRef = React.useRef(null);
  const wrapperRef = React.useRef(null);
  useOnClickOutside([dropDownRef, wrapperRef], closeDropDown);

  return (
    <>
      <div
        className={`${styles.shareSection} dropdown-toggle`}
        aria-expanded="false"
        onClick={toggleDropDown}
        ref={wrapperRef}
      >
        <PixelHeart className="img-responsive" width={32} height={20} />
      </div>
      <Notifications data={consoleNotifications} ref={dropDownRef} />
    </>
  );
};

interface NotificationData {
  main: {
    consoleNotifications: Array<ConsoleNotification>;
  };
}

function mapStateToProps(state: NotificationData) {
  return {
    consoleNotifications: state.main.consoleNotifications,
  };
}

export default connect(mapStateToProps)(LoveSection);
