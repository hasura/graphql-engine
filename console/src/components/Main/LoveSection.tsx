import React, { useState, useEffect } from 'react';
import { css } from 'styled-components';
import fetch from 'isomorphic-fetch';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import styles from './Main.scss';
import Endpoints from '../../Endpoints';
import PixelHeart from './images/components/PixelHeart';
import ConsoleLogo from './images/components/ConsoleLogo';

const getDateString = (date: string | number | Date) => {
  return new Date(date).toLocaleString().split(', ')[0];
};

type UpdateProps = {
  subject: string;
  created_at: string;
  content: string;
  type: string;
  id?: number;
  is_active?: boolean;
  external_link?: string;
};

const Update: React.FC<UpdateProps> = ({
  subject,
  created_at,
  content,
  type,
  is_active,
}) => {
  if (!is_active) {
    return null;
  }

  return (
    <Box>
      <Flex
        height={55}
        px="25px"
        pt="5px"
        justifyContent="space-between"
      >
        <Flex justifyContent="space-between" bg="white">
          <Badge type={type} mr="12px" />
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
        </Text>
        {/* TODO: add support for external links */}
      </Flex>
    </Box>
  );
};

type NotificationProps = {
  data: Array<UpdateProps>;
};

const Notifications: React.FC<NotificationProps> = ({ data }) => (
  <Box
    className="dropdown-menu"
    // TODO: remove these inline styles
    css={css`
      width: 520px;
      box-shadow: 3px;
      margin: 0;
      padding: 0;
      background: white;
      text-transform: none;
      left: auto;
      #close-icon {
        &:hover {
          color: #000 !important;
        }
      }
      #console-logo {
        width: 20px;
        margin-top: -3px;
        margin-left: 8px;
      }
      #update-link {
        border-bottom: 1px solid transparent;
        &:hover {
          color: #e53935;
          border-bottom: 1px solid #e53935;
        }
      }
    `}
  >
    <Flex justifyContent="space-between" px={20} py={3}>
      <Heading as="h2" color="#000" fontSize="20px">
        Latest updates
        <ConsoleLogo id="console-logo" />
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
          external_link={props.external_link ? props.external_link : ''}
        />
      ))}
  </Box>
);

const defaultNotification: UpdateProps = {
  subject: '',
  created_at: getDateString(Date.now()),
  content:
    "You're all caught up! \n There are no updates available at this point in time.",
  type: 'No Updates',
};

const LoveSection = () => {
  const [open, toggleLove] = useState(false);
  const [notificationData, setData] = useState([defaultNotification] as Array<
    UpdateProps
  >);

  const reqBody = {
    args: {
      limit: 5,
      table: 'console_notification',
      where: null,
      offset: null,
      columns: ['*'],
      order_by: [
        {
          type: 'desc',
          column: ['id'],
        },
      ],
    },
    type: 'select',
  };

  useEffect(() => {
    if (open) {
      fetch(Endpoints.checkNotifications, {
        method: 'POST',
        body: JSON.stringify(reqBody),
      })
        .then(response => response.json())
        .then(data => {
          if (data.length) {
            setData(data);
          }
        })
        // FIXME: report error in a better way
        .catch(err => console.error(err));
    }
    const dropDown: HTMLElement | null = document.getElementById(
      'dropdown_wrapper'
    );
    if (dropDown) {
      dropDown.classList.toggle('open');
    }
  }, [open]);

  return (
    <>
      <div
        className={`${styles.shareSection} dropdown-toggle`}
        aria-expanded="false"
        // TODO: use state instead of classnames for toggling
        onClick={() => toggleLove(!open)}
      >
        <PixelHeart className="img-responsive" width={32} height={20} />
      </div>
      <Notifications data={notificationData} />
    </>
  );
};

export default LoveSection;
