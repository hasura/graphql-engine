import React, { useState, useLayoutEffect } from 'react';
import { css } from 'styled-components';
import fetch from 'isomorphic-fetch';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import styles from './Main.scss';
import Endpoints from '../../Endpoints';

const pixHeart = require('./images/pix-heart.svg');
const consoleLogo = require('./images/console-logo.svg');

type UpdateProps = {
  title: string;
  time: string;
  description: string;
  badge: string;
};

const Update: React.FC<UpdateProps> = ({ title, time, description, badge }) => (
  <Box>
    <Flex
      height={55}
      px="25px"
      pt="5px"
      borderBottom="1px solid #e1e1e1"
      justifyContent="space-between"
    >
      <Flex justifyContent="space-between">
        <Badge type={badge} mr="12px" />
        <Heading as="h4" color="#1cd3c6" fontSize="16px">
          {title}
        </Heading>
      </Flex>
      <Text color="grey" fontSize={13} fontWeight="medium" type={0}>
        {time}
      </Text>
    </Flex>
    <Text fontSize={15} fontWeight="normal" px={25} py={8}>
      {description}
    </Text>
  </Box>
);

type NotificationProps = {
  data: Array<UpdateProps>;
};

const Notifications: React.FC<NotificationProps> = ({ data }) => (
  <Box
    className="dropdown-menu"
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
    <Flex justifyContent="space-between">
      <Heading as="h2" color="#000" fontSize="20px">
        Latest updates
        <img src={consoleLogo} alt="hasura-console" id="console-logo" />
      </Heading>
    </Flex>
    {data.length &&
      data.map(({ title, time, description, ...props }) => (
        <Update
          key={title}
          title={title}
          time={time}
          description={description}
          badge={props.badge ? props.badge : ''}
        />
      ))}
  </Box>
);

const getCurrentDate = () => {
  return new Date(Date.now()).toLocaleString().split(', ')[0];
};

const LoveSection = () => {
  const defaultNotification: UpdateProps = {
    title: '',
    time: getCurrentDate(),
    description:
      "You're all caught up! \n There are no updates available at this point in time.",
    badge: 'No Updates',
  };
  const [open, toggleLove] = useState(false);
  const [notificationData, setData] = useState([defaultNotification] as Array<UpdateProps>);

  useLayoutEffect(() => {
    if (open) {
      fetch(Endpoints.checkNotifications)
        .then(response => response.json())
        .then(data => {
          console.log(data);

          // FIXME: this conditional check
          // until the table is fixed
          if (data.length > 1) {
            const notifData: Array<UpdateProps> = [];
            // TODO: Process the response here
            // and push to notifData
            setData(notifData);
          }

        })
        // TODO: report error in a better way
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
        onClick={() => toggleLove(!open)}
      >
        <img className="img-responsive" src={pixHeart} alt="pix Heart" />
      </div>
      <Notifications data={notificationData} />
    </>
  );
};

export default LoveSection;
