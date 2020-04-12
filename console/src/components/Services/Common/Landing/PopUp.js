import React from 'react';
import PropTypes from 'prop-types';

import TextAreaWithCopy from '../../../Common/TextAreaWithCopy/TextAreaWithCopy';
import RemoteSchemaContent from './RemoteSchemaContent';
import EventTriggerContent from './EventTriggerContent';
import close from './images/cancel.svg';
import { Heading, Text } from '../../../UIKit/atoms';
import styles from './Popup.scss';

const ContentMap = {
  remoteSchema: <RemoteSchemaContent styles={styles} />,
  eventTrigger: <EventTriggerContent styles={styles} />
};

const PopUp = ({
  onClose,
  service,
  title,
  queryDefinition,
  footerDescription,
  isAvailable
}) => {
  // const queryDefinition = 'query { hello }';
  const commonPopupStyle = isAvailable
    ? styles.popupWrapper
    : styles.popupWrapperPos;

  return (
    <div className={commonPopupStyle}>
      <div className={styles.wd100}>
        <Heading
          type='subHeading'
          fontSize='16px'
          pb='18px'
          borderBottom={1}
          textAlign='left'
          borderColor='black.hover'
        >
          {title}
        </Heading>
        <div className={styles.close} onClick={onClose}>
          <img className={'img-responsive'} src={close} alt={'Close'} />
        </div>
        {isAvailable && <div className={styles.arrowLeft} />}
        {ContentMap[service]}
        <div className={styles.addPaddLeft + ' text-left ' + styles.addPaddTop}>
          <TextAreaWithCopy
            copyText={queryDefinition}
            textLanguage={'graphql'}
            id={'copyQueryDefinition'}
          />
        </div>
        <div className={styles.listItems}>
          <Text pl='20px' textAlign='left'>
            {footerDescription}
          </Text>
        </div>
      </div>
    </div>
  );
};

PopUp.propTypes = {
  onClose: PropTypes.func.isRequired,
  title: PropTypes.string.isRequired,
  queryDefinition: PropTypes.string.isRequired,
  footerDescription: PropTypes.string.isRequired,
  service: PropTypes.string.isRequired
};

export default PopUp;
