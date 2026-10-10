import styles from './Popup.module.scss';
import close from './images/cancel.svg';
import React from 'react';
import { RemoteSchemaContent } from './RemoteSchemaContent';
import { EventTriggerContent } from './EventTriggerContent';
import { GraphqlCodeBlock } from '../components';

const ContentMap = {
  remoteSchema: <RemoteSchemaContent styles={styles} />,
  eventTrigger: <EventTriggerContent styles={styles} />,
};

export type PopupContentKey = keyof typeof ContentMap;

type Props = {
  onClose: () => void;
  title: string;
  queryDefinition: string;
  footerDescription: React.ReactNode;
  service: PopupContentKey;
  isAvailable?: boolean;
};

const PopUp = ({
  onClose,
  title,
  queryDefinition,
  footerDescription,
  isAvailable,
  service,
}: Props) => {
  const commonPopupStyle = isAvailable
    ? styles.popupWrapper
    : styles.popupWrapperPos;
  const isAvailableText = isAvailable ? (
    <div className={styles.arrowLeft} />
  ) : null;

  return (
    <div className={commonPopupStyle}>
      <div className={styles.wd100}>
        <div
          className={
            styles.descriptionText +
            ' ' +
            styles.fontWeightBold +
            ' ' +
            styles.addPaddBottom +
            ' ' +
            styles.commonBorBottom
          }
        >
          {title}
        </div>
        <div className={styles.close} onClick={onClose}>
          <img className={'img-responsive'} src={close} alt={'Close'} />
        </div>
        {isAvailableText}
        {ContentMap[service]}
        <div className={styles.addPaddLeft + ' text-left ' + styles.addPaddTop}>
          <GraphqlCodeBlock text={queryDefinition} />
        </div>
        <div className={styles.listItems}>
          <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
            {footerDescription}
          </div>
        </div>
      </div>
    </div>
  );
};

export default PopUp;
