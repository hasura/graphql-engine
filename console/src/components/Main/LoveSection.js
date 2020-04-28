import React from 'react';
import { css } from 'styled-components';

import globals from '../../Globals';
import pixHeart from './images/pix-heart.svg';
import consoleLogo from './images/console-logo.svg';
import arrowForwardRed from './images/arrow_forward-red.svg';
import {
  Icon,
  Box,
  Flex,
  Heading,
  TextLink,
  Text,
  Badge,
} from '../UIKit/atoms';
import styles from './Main.scss';

// eslint-disable-next-line no-unused-vars
const OldLoveSection = ({ toggleDropdown }) => (
  <ul className={'dropdown-menu ' + styles.dropdown_menu}>
    <div className={styles.dropdown_menu_container}>
      <Icon
        type="close"
        position="absolute"
        pointer
        color="black.secondary"
        top="10px"
        left="20px"
        onClick={toggleDropdown}
      />
      <div className={styles.displayFlex}>
        <li className={styles.pixelText1}>
          Roses are red, <br />
          Violets are blue;
          <br />
          Star us on GitHub,
          <br />
          To make our <Icon type="love" size={10} mx="xs" /> go wooooo!
        </li>
        <li className={'dropdown-item'}>
          <a
            href="https://github.com/hasura/graphql-engine"
            target="_blank"
            rel="noopener noreferrer"
          >
            <div className={styles.socialIcon}>
              <img
                className="img img-responsive"
                src={`${globals.assetsPath}/common/img/githubicon.png`}
                alt={'GitHub'}
              />
            </div>
            <div className={styles.pixelText}>
              <Icon type="star" size={12} mr="5px" />
              Star
            </div>
          </a>
        </li>
        <li className={'dropdown-item '}>
          <a
            href="https://twitter.com/intent/tweet?hashtags=graphql,postgres&text=Just%20deployed%20a%20GraphQL%20backend%20with%20@HasuraHQ!%20%E2%9D%A4%EF%B8%8F%20%F0%9F%9A%80%0Ahttps://github.com/hasura/graphql-engine%0A"
            target="_blank"
            rel="noopener noreferrer"
          >
            <div className={styles.socialIcon}>
              <img
                className="img img-responsive"
                src={`${globals.assetsPath}/common/img/twittericon.png`}
                alt={'Twitter'}
              />
            </div>
            <div className={styles.pixelText}>
              <Icon type="twitter" size={12} mr="5px" />
              Tweet
            </div>
          </a>
        </li>
      </div>
    </div>
  </ul>
);

const updateData = [
  {
    title: 'HasuraCon, Online',
    time: '12 May, 2020',
    description: 'HasuraCon is happening at the Hasura office this month.',
    badge: 'event',
  },
  {
    title: 'GraphQL Asia, Bengaluru',
    time: '20 Feb, 2020',
    description:
      "GraphQL Asia is the largest gathering of the GraphQL community in Asia with over 500 people expected at 2020 edition, you'll meet the creators of leading GraphQL tooling, talk to the experts implementing GraphQL in their companies as well as practicioners and enthusiasts from Asia and the rest of the world.",
    badge: 'community',
  },
  {
    title: 'v1.2.0-beta.5',
    time: '24 April, 2020',
    description:
      'We fixed some major bugs, added some new features and made life generally easier. Check it out!',
    badge: 'update',
  },
];

const Update = ({ title, time, description, badge }) => (
  <Box borderBottom={1}>
    <Flex height="55px" justifyContent="space-between" px="25px" pt="5px">
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
    <Text fontSize="15px" fontWeight="normal" px="25px" pb="20px">
      {description}
    </Text>
  </Box>
);

const NewLoveSection = ({ toggleDropdown }) => (
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
    <Flex
      justifyContent="space-between"
      px="25px"
      py="22px"
      borderBottom={1}
      bg="#f8f8f8"
    >
      <Heading as="h2" color="#000" fontSize="20px">
        Latest updates
        <img src={consoleLogo} alt="hasura-console" id="console-logo" />
      </Heading>
      <Icon
        type="close"
        color="#5a5a5a"
        id="close-icon"
        size={16}
        pointer
        onClick={toggleDropdown}
      />
    </Flex>
    {updateData &&
      updateData.length >= 1 &&
      updateData.map(({ title, time, description, badge }) => (
        <Update
          key={title}
          title={title}
          time={time}
          description={description}
          badge={badge}
        />
      ))}
    <Flex px="25px" py="20px" color="red.primary">
      <TextLink href="#" id="update-link">
        View all updates{' '}
        <img className={styles.arrow} src={arrowForwardRed} alt={'Arrow'} />
      </TextLink>
    </Flex>
  </Box>
);

const LoveSection = () => {
  const toggleDropdown = () => {
    document.getElementById('dropdown_wrapper').classList.toggle('open');
  };

  return (
    <>
      <div
        className={styles.shareSection + ' dropdown-toggle'}
        aria-expanded="false"
        onClick={toggleDropdown}
      >
        <img className={'img-responsive'} src={pixHeart} alt={'pix Heart'} />
      </div>
      {/* <OldLoveSection toggleDropdown={toggleDropdown} /> */}
      <NewLoveSection toggleDropdown={toggleDropdown} />
    </>
  );
};

export default LoveSection;
