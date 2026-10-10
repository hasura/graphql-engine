import React, { useState } from 'react';
import PopUp, { PopupContentKey } from './PopUp';
import Rectangle from './images/Rectangle.svg';
import styles from './RemoteSchema.module.scss';
import glitch from './images/glitch.png';
import googleCloud from './images/google_cloud.svg';
import MicrosoftAzure from './images/Microsoft_Azure_Logo.svg';
import AWS from './images/AWS.png';
import { Button, Card, Link, Separator, Text } from '../components';
import { Flex, Strong } from '@radix-ui/themes';
import { FaExternalLinkAlt } from 'react-icons/fa';
import { BsCaretRightFill } from 'react-icons/bs';

type Props = {
  title: string;
  service: PopupContentKey;
  queryDefinition: string;
  glitchLink: string;
  googleCloudLink: string;
  microsoftAzureLink: string;
  awsLink: string;
  adMoreLink: string;
  footerDescription: React.ReactNode;
  isAvailable?: boolean;
};

export const TryItOut = ({
  isAvailable,
  glitchLink,
  service,
  queryDefinition,
  title,
  footerDescription,
  googleCloudLink,
  microsoftAzureLink,
  awsLink,
  adMoreLink,
}: Props) => {
  const [isPopUp, setIsPopUp] = useState(false);

  const togglePopup = () => {
    setIsPopUp(!isPopUp);
  };

  return (
    <div>
      <Flex align="center" gap="2">
        <img className={'img-responsive'} src={Rectangle} alt={'Rectangle'} />
        <Text weight="bold">Try it out</Text>
      </Flex>
      <Flex align="center" gap="5" className="my-8">
        <Card className={styles.boxLarge}>
          <div className={styles.logoIcon}>
            <img className={'img-responsive'} src={glitch} alt={'glitch'} />
          </div>
          <Link
            href={glitchLink}
            target="_blank"
            rel="noopener noreferrer"
            underline="none"
          >
            <Button rightIcon={FaExternalLinkAlt}>Try it with Glitch </Button>
          </Link>
          <Separator size="4" className="my-4" />
          <Flex justify="center" align="center">
            <Flex align="center" justify="center" onClick={togglePopup} gap="1">
              <Text>Instructions</Text>
              <BsCaretRightFill />
            </Flex>
            {isPopUp ? (
              <PopUp
                onClose={togglePopup}
                service={service}
                title={title}
                queryDefinition={queryDefinition}
                footerDescription={footerDescription}
                isAvailable={isAvailable}
              />
            ) : null}
          </Flex>
        </Card>
        <a
          href={googleCloudLink}
          target={'_blank'}
          rel="noopener noreferrer"
          title={'Google Cloud'}
        >
          <Card className={styles.boxSmall}>
            <div className={styles.logoIcon}>
              <img
                className={'img-responsive'}
                src={googleCloud}
                alt={'Google Cloud'}
              />
            </div>
          </Card>
        </a>
        <a
          href={microsoftAzureLink}
          target={'_blank'}
          rel="noopener noreferrer"
          title={'Microsoft Azure'}
        >
          <Card className={styles.boxSmall}>
            <div className={styles.logoIcon}>
              <img
                className={'img-responsive'}
                src={MicrosoftAzure}
                alt={'Microsoft Azure'}
              />
            </div>
          </Card>
        </a>
        <a
          href={awsLink}
          target={'_blank'}
          rel="noopener noreferrer"
          title={'AWS'}
        >
          <Card className={styles.boxSmall}>
            <div className={styles.logoIcon}>
              <img
                className={'img-responsive ' + styles.imgAws}
                src={AWS}
                alt={'AWS'}
              />
            </div>
          </Card>
        </a>
        <Link
          underline="none"
          href={adMoreLink}
          target="_blank"
          rel="noopener noreferrer"
          color="gray"
        >
          <Strong>And many more</Strong> <BsCaretRightFill />
        </Link>
      </Flex>
    </div>
  );
};
