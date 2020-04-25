import React from 'react';

import globals from '../../Globals';
import pixHeart from './images/pix-heart.svg';
import { Icon } from '../UIKit/atoms';
import styles from './Main.scss';

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
    </>
  );
};

export default LoveSection;
