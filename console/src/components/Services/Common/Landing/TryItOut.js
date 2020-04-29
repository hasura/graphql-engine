import React, { useState } from 'react';
import PropTypes from 'prop-types';

import PopUp from './PopUp';
import Rectangle from './images/Rectangle.svg';
import glitch from './images/glitch.png';
import googleCloud from './images/google_cloud.svg';
import MicrosoftAzure from './images/Microsoft_Azure_Logo.svg';
import AWS from './images/AWS.png';
import externalLink from './images/external-link.svg';
import { Heading, Link, Icon } from '../../../UIKit/atoms';
import styles from '../../RemoteSchema/RemoteSchema.scss';

const TryItOut = ({
  isAvailable,
  glitchLink,
  service,
  title,
  googleCloudLink,
  queryDefinition,
  footerDescription,
  MicrosoftAzureLink,
  awsLink,
  adMoreLink,
}) => {
  const [isPopUp, setPopUp] = useState(false);

  const togglePopup = () => setPopUp(!isPopUp);

  const commonStyle = isAvailable
    ? styles.instructionsWrapper
    : styles.instructionsWrapperPos;

  return (
    <div>
      <div className={styles.subHeaderText}>
        <img className={'img-responsive'} src={Rectangle} alt={'Rectangle'} />
        <Heading as="h4" display="inline-block" fontSize="18px" fontWeight={6}>
          Try it out
        </Heading>
      </div>
      <div className={styles.tryOutWrapper}>
        <div className={styles.boxLarge}>
          <div className={styles.logoIcon}>
            <img className={'img-responsive'} src={glitch} alt={'glitch'} />
          </div>
          <Link href={glitchLink} target="_blank">
            <button className={styles.default_button}>
              Try it with Glitch{' '}
              <img
                className={'img-responsive ' + styles.externalLinkImg}
                src={externalLink}
                alt={'externalLink'}
              />
            </button>
          </Link>
          <div className={styles.displayFlex + ' ' + commonStyle}>
            <span
              onClick={togglePopup}
              className={styles.instructions + ' ' + styles.displayFlex}
            >
              <span>Instructions</span>
              <Icon type="play" ml="12px" mb="-2px" size={10} />
            </span>
            {isPopUp && (
              <PopUp
                onClose={togglePopup}
                service={service}
                title={title}
                queryDefinition={queryDefinition}
                footerDescription={footerDescription}
                isAvailable={isAvailable}
              />
            )}
          </div>
        </div>
        <div className={styles.boxSmallWrapper}>
          <Link href={googleCloudLink} target={'_blank'} title={'Google Cloud'}>
            <div className={styles.boxSmall}>
              <div className={styles.logoIcon}>
                <img
                  className={'img-responsive'}
                  src={googleCloud}
                  alt={'Google Cloud'}
                />
              </div>
            </div>
          </Link>
          <Link
            href={MicrosoftAzureLink}
            target={'_blank'}
            title={'Microsoft Azure'}
          >
            <div className={styles.boxSmall}>
              <div className={styles.logoIcon}>
                <img
                  className={'img-responsive'}
                  src={MicrosoftAzure}
                  alt={'Microsoft Azure'}
                />
              </div>
            </div>
          </Link>
          <Link href={awsLink} target={'_blank'} title={'AWS'}>
            <div className={styles.boxSmall}>
              <div className={styles.logoIcon}>
                <img
                  className={'img-responsive ' + styles.imgAws}
                  src={AWS}
                  alt={'AWS'}
                />
              </div>
            </div>
          </Link>
          <div className={styles.instructions}>
            <Link href={adMoreLink} target="_blank" fontWeight="bold">
              And many more
              <Icon type="play" ml="12px" size={10} />
            </Link>
          </div>
        </div>
      </div>
    </div>
  );
};

TryItOut.propTypes = {
  service: PropTypes.string.isRequired,
  queryDefinition: PropTypes.string.isRequired,
  glitchLink: PropTypes.string.isRequired,
  googleCloudLink: PropTypes.string.isRequired,
  MicrosoftAzureLink: PropTypes.string.isRequired,
  awsLink: PropTypes.string.isRequired,
  adMoreLink: PropTypes.string.isRequired,
  // imgUrl: PropTypes.string.isRequired,
  // imgAlt: PropTypes.string.isRequired,
  // description: PropTypes.string.isRequired,
};

export default TryItOut;
