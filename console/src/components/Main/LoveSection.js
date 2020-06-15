import React, { useState, useLayoutEffect } from 'react';
import { css } from 'styled-components';

import pixHeart from './images/pix-heart.svg';
import consoleLogo from './images/console-logo.svg';
import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import styles from './Main.scss';
import fetch from 'isomorphic-fetch';
import Endpoints from '../../Endpoints';

const Update = ({ title, time, description, badge }) => (
  <Box>
    <Flex
      height="55px"
      justifyContent="space-between"
      px="25px"
      pt="5px"
      borderBottom={'1px solid #e1e1e1'}
    >
      <Flex>
        <Badge type={badge} mr="12px" />
        <Heading as="h4" color="#1cd3c6" fontSize="16px">
          {title}
        </Heading>
      </Flex>
      <Text
        color="#acacac"
        fontSize="13px"
        fontWeight="medium"
        fontFamily="roboto"
        letterSpacing="0.25px"
      >
        {time}
      </Text>
    </Flex>
    <Text fontSize="15px" fontWeight="normal" px="25px" py="8px">
      {description}
    </Text>
  </Box>
);

const NoNotification = () => <p>This is a text field</p>;

const Notifications = ({ data }) => (
  <Box
    className="dropdown-menu"
    border={0}
    width="520px"
    boxShadow={3}
    margin={0}
    bg="white"
    padding="0px"
    left="auto"
    css={css`
      text-transform: none;
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
    <Flex justify="space-between" borderBottom={1} bg="#f8f8f8">
      <Heading as="h2" color="#000" fontSize="20px">
        Latest updates
        <img src={consoleLogo} alt="hasura-console" id="console-logo" />
      </Heading>
    </Flex>
    {data.length ? (
      data.map(({ title, time, description, ...props }) => (
        <Update
          key={title}
          title={title}
          time={time}
          description={description}
          badge={props.badge ? props.badge : null}
        />
      ))
    ) : (
      <NoNotification />
    )}
  </Box>
);

const getCurrentDate = () => {
  return new Date(Date.now()).toLocaleString().split(', ')[0];
};

const LoveSection = () => {
  const [open, toggleLove] = useState(false);
  const [notificationData, setData] = useState([
    {
      title: 'No New Updates',
      time: getCurrentDate(),
      description:
        "You're all caught up! \n There are no updates available at this point in time.",
    },
  ]);

  useLayoutEffect(() => {
    if (open) {
      fetch(Endpoints.checkNotifications)
        .then(response => response.json())
        .then(data => setData(data))
        // TODO: report error in a better way
        .catch(err => console.error(err));
    }
    document.getElementById('dropdown_wrapper').classList.toggle('open');
  }, [open]);

  return (
    <>
      <div
        className={styles.shareSection + ' dropdown-toggle'}
        aria-expanded="false"
        onClick={() => toggleLove(!open)}
      >
        <img className="img-responsive" src={pixHeart} alt={'pix Heart'} />
      </div>
      <Notifications data={notificationData} />
    </>
  );
};

export default LoveSection;
